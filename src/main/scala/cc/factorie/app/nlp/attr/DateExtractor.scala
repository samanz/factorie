package cc.factorie.app.nlp.attr

import cc.factorie.app.nlp.mention.Mention
import java.util.{GregorianCalendar, Calendar, Date}
import scala.collection.mutable.ArrayBuffer
import cc.factorie.app.nlp.{Section, Document}
import cc.factorie.app.nlp.segment.{SentenceSegmenter1, Tokenizer1}
import scala.io.Source
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

/**
 * @author anzaroot
 */

class Specificity(val Day : Boolean = false, val Month : Boolean = false, val Year : Boolean = false) {
  val full = Day && Month && Year
}

object Specificity {
  def fullDay = new Specificity(true,true,true)
  def fullMonth = new Specificity(false,true,true)
  def fullYear = new Specificity(Year=true)
  def fromTuple(indices : (Int,Int,Int)) = new Specificity(Year = indices._1 >= 0, Month = indices._2 >= 0, Day = indices._3 >= 0)
}

/**
 * DateMention is a mention of an extracted date.
 */
case class DateMention(mention : Mention, specificity : Specificity, calendar : Calendar = new GregorianCalendar(1900,0,1), diff : Option[DateDiff] = None) {
  lazy val _date : Date = {
    if(specificity.full && !diff.isDefined) calendar.getTime
    else null
  }
  def date : Option[Date] = {
    if(mention.section.attr.contains[DateMention] || (specificity.full && !diff.isDefined)) Some(_date)
    else None
  }
  def contains(rhs: DateMention) = mention.start <= rhs.mention.start && rhs.mention.end <= mention.end
}

class DateDiff(val unit : Int, val diff : Int, val dayOfWeek : Int = -1)

class DateMentionList extends ArrayBuffer[DateMention]
/**
 * DateMentionFinding provides two functions. The first is that if a document does not have sections with DateMentions,
 * it can optionally populate the section attrs with DateMentions based on the first fully specified DateMention in the doc.
 * The second is that it returns a list of DateMentions with various specificity. If a date mentions does not have
 * full specificity the rest of the date information is determined from the section date information.
 */
object DateMentionFinding {

  val months = Seq("January", "Febuary", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  val monthsAbbr = months.map(_.take(3)) ++ Seq("Sept")
  val allMonths = months ++ monthsAbbr
  val daysOfWeek = Seq("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  val daysOfWeekAbbr = daysOfWeek.map(_.take(3)) ++ Seq("Tues", "Thurs")
  val daysOfWeekAll = daysOfWeek ++ daysOfWeekAbbr
  val temporal = Seq(("last", -1), ("Last", -1), ("next",1), ("Next",1))
  val dayTemporal = Seq( ("today", 0), ("Today", 0), ("tomorrow", 1), ("Tomorrow", 1), ("yesterday",-1), ("Yesterday",-1))
  val validDays = 1 to 31
  val validDaysTwo = validDays.map{ a => (if(a < 10) "0" else "") + a.toString }
  val validDaysAll = validDays ++ validDaysTwo
  val validMonths = 1 to 12
  val validMonthsTwo = validMonths.map{ a => (if(a < 10) "0" else "") + a.toString }
  val validMonthsAll = validMonths ++ validMonthsTwo
  val validYears = 1600 to 2100
  val validYearTwo = (0 until 100).map{ a => (if(a < 10) "0" else "") + a.toString }
  val validYearsAll = validYears ++ validYearTwo
  val namedNumbers = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")

  val MDYFull = (s"((${daysOfWeekAll.mkString("|")}),? )?(${allMonths.mkString("|")})\\.?,? (\\d{1,2})(nd|st|th)?,? '?(${validYearsAll.mkString("|")})").r
  val DMYFull = (s"((${daysOfWeekAll.mkString("|")}),? )?(\\d{1,2})(nd|st|th)?,? (${allMonths.mkString("|")})\\.? '?(${validYearsAll.mkString("|")})").r
  val MDFull =  (s"((${daysOfWeekAll.mkString("|")}),? )?(${allMonths.mkString("|")})\\.? (\\d{1,2})((nd|st|th)?),?").r
  val MFull = (s"(${allMonths.mkString("|")})\\.?").r
  val YFull = (s"(${validYears.mkString("|")})").r
  val MYFull = (s"(${allMonths.mkString("|")})\\.? '?(${validYearsAll.mkString("|")})").r

  val MDYsep = (s"(${validMonthsAll.mkString("|")})(/|-)(${validDaysAll.mkString("|")})(/|-)(${validYearsAll.mkString("|")})").r
  val DMYsep = (s"(${validDaysAll.mkString("|")})(/|-)(${validMonthsAll.mkString("|")})(/|-)(${validYearsAll.mkString("|")})").r
  val YMDsep = (s"(${validYears.mkString("|")})(/|-)(${validMonthsAll.mkString("|")})(/|-)(${validDaysAll.mkString("|")})").r

  val dayMatch =   (s"(${daysOfWeekAll.mkString("|")})").r

  val tomOrToday = (s"(${dayTemporal.mkString("|")}),?").r

  val ago = (s"(\\d|a|A|${namedNumbers.mkString("|")}) (days|day|months|month|week|weeks|year|years) ago").r

  val nextLast = (s"(${temporal.map(_._1).mkString("|")}) (week|year|month|${daysOfWeekAll.mkString("|")})").r

  val regexs = Seq(MDYFull,DMYFull,MDFull,MFull,MYFull,MDYsep,DMYsep,YMDsep,dayMatch,tomOrToday, ago, nextLast)

  def process(document : Document, extractSectionDates : Boolean = false) : DateMentionList = {
    val docMentionList = new DateMentionList()
    document.sections.foreach( docMentionList ++= process(_))
    if(document.sections.exists(!_.attr.contains[DateMention]) && extractSectionDates) {
      docMentionList.find( _.specificity.full ) match {
        case Some(a) => document.sections.foreach( _.attr += a )
        case None =>  Nil
      }
    }
    docMentionList
  }

  def processRegex(regex : Regex, section : Section)(createMention : (Match,Mention) => DateMention) : DateMentionList = {
    val list = new DateMentionList()
    regex.findAllMatchIn(section.string).foreach{ m =>
      val token = section.tokens.find( _.stringStart+section.stringStart == m.start)
      token.foreach{ t =>
        val last = section.tokens.find( _.stringEnd + section.stringStart == m.end)
        last.foreach{ l =>
          list += createMention(m, new Mention(section, t.positionInSection, l.positionInSection-t.positionInSection+1))
        }
      }
    }
    list
  }

  def namedMonthFull(section : Section, r : Regex, indices : (Int, Int,Int)) : DateMentionList = {
    processRegex(r,section) { (m, mention) =>
      val month = if(indices._2 >= 0) { if(m.group(indices._2).length > 4) months.indexOf(m.group(indices._2)) else if(m.group(indices._2).length == 3) monthsAbbr.indexOf(m.group(indices._2)) else 8 } else 0
      val day = if(indices._3 >= 0) m.group(indices._3).toInt else 1
      val year = if(indices._1 >= 0) { if(m.group(indices._1).length == 2 && m.group(indices._1).toInt > 25) ("19" + m.group(indices._1)).toInt else if(m.group(indices._1).length == 2 && m.group(indices._1).toInt <= 25) ("20" + m.group(indices._1)).toInt else m.group(indices._1).toInt } else 1900
      new DateMention(mention, Specificity.fromTuple(indices), new GregorianCalendar(year, month, day))
    }
  }

  def sep(section : Section, r : Regex, indices : (Int,Int,Int)) : DateMentionList  = {
    processRegex(r,section) { (m, mention) =>
          val month = if(m.group(indices._2).startsWith("0")) m.group(indices._2).drop(1).toInt else m.group(indices._2).toInt - 1
          val day = if(m.group(indices._3).startsWith("0")) m.group(indices._3).drop(1).toInt else m.group(indices._3).toInt
          val year = if(m.group(indices._1).length == 2 && m.group(indices._1).toInt > 25) ("19" + m.group(indices._1)).toInt else if(m.group(indices._1).length == 2 && m.group(indices._1).toInt <= 25) ("20" + m.group(indices._1)).toInt else m.group(indices._1).toInt
          new DateMention(mention, Specificity.fullDay, new GregorianCalendar(year, month, day))
    }
  }

  def getCategory(string : String) : Int = {
    if(string.toLowerCase == "day" || string.toLowerCase == "days") Calendar.DAY_OF_YEAR
    else if(string.toLowerCase == "week" || string.toLowerCase == "weeks") Calendar.WEEK_OF_YEAR
    else if(string.toLowerCase == "month" || string.toLowerCase == "months") Calendar.MONTH
    else if(string.toLowerCase == "year" || string.toLowerCase == "years") Calendar.YEAR
    else if(string.toLowerCase == "monday") Calendar.MONDAY
    else if(string.toLowerCase == "tuesday") Calendar.TUESDAY
    else if(string.toLowerCase == "wednesday") Calendar.WEDNESDAY
    else if(string.toLowerCase == "thursday") Calendar.THURSDAY
    else if(string.toLowerCase == "friday") Calendar.FRIDAY
    else if(string.toLowerCase == "saturday") Calendar.SATURDAY
    else if(string.toLowerCase == "sunday") Calendar.SUNDAY
    else -1
  }

  def process(section : Section) : DateMentionList = {
    val list = new DateMentionList()

    list ++= namedMonthFull(section, MDYFull, (6,3,4))
    list ++= namedMonthFull(section, DMYFull, (6,5,3))
    list ++= namedMonthFull(section, MDFull, (-1,3,4))
    list ++= namedMonthFull(section, MFull, (-1,1,-1))
    list ++= namedMonthFull(section, YFull, (1,-1,-1))
    list ++= namedMonthFull(section, MYFull, (2,1,-1))

    list ++= sep(section, MDYsep, (5,1,3))
    list ++= sep(section, DMYsep, (5,3,1))
    list ++= sep(section, YMDsep, (1,3,5))

    list ++= processRegex(nextLast, section) { (m,mention) =>
      new DateMention(mention, if(m.group(2) == "month") Specificity.fullMonth else if(m.group(2) == "year") Specificity.fullYear else Specificity.fullDay, diff = Some(new DateDiff(getCategory(m.group(2)),temporal.find(_._1 == m.group(1)).head._2)))
    }
    list ++= processRegex(dayMatch, section) { (m,mention) =>
      new DateMention(mention, Specificity.fullDay, diff = Some(new DateDiff(getCategory(m.group(1)),0)))
    }
    list ++= processRegex(tomOrToday, section) { (m,mention) =>
      new DateMention(mention, Specificity.fullDay, diff = Some(new DateDiff(getCategory(m.group(1)),0)))
    }
    list ++= processRegex(ago, section) { (m,mention) =>
      val diff = if(m.group(1) == "A" || m.group(1) == "a") -1 else if(namedNumbers.contains(m.group(1))) -1*(namedNumbers.indexOf(m.group(1)) + 1) else -1*m.group(1).toInt
      val unit = getCategory(m.group(2))
      new DateMention(mention, Specificity.fullDay, diff = Some(new DateDiff(unit,diff)))
    }

    filterOverlapping(list)
  }

  def filterOverlapping(list : DateMentionList) : DateMentionList = {
    val newList = list.sortBy(_.mention.start).foldLeft(List.empty[DateMention]) { (acc, r) =>
      acc match {
        case head :: tail if head.contains(r) =>
          head :: tail
        case _ =>
          r :: acc
      }
    }
    val mentionList = new DateMentionList
    newList.foreach( mentionList += _)
    mentionList
  }

  def main(args : Array[String]) {
    val doc = new Document(Source.fromFile(args(0)).mkString)
    Tokenizer1.process(doc)
    SentenceSegmenter1.process(doc)
    //println(doc.string)
    val dates = process(doc, extractSectionDates = true).sortBy(_.mention.start)
    for(date <- dates) {
      println(date.mention.phrase)
      println(date.calendar.getTime)
    }
  }

}
