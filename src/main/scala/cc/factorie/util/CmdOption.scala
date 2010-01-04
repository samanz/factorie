/* Copyright (C) 2008-2009 Univ of Massachusetts Amherst, Computer Science Dept
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   This software is provided under the terms of the Eclipse Public License 1.0
   as published by http://www.opensource.org.  For further information,
   see the file `LICENSE.txt' included with this distribution. */

package cc.factorie.util
import scala.reflect.Manifest
import scala.collection.mutable.{HashMap,ArrayBuffer}


/** Concrete version is implemented as an inner class of @see CmdOptions. 
    @author Andrew McCallum */
trait CmdOption[T] {
  def name: String
  def shortName: Char
  def helpMsg: String
  def valueClass: Class[_];
  def valueName: String
  def defaultValue: T
  def value: T
  def invokedCount: Int
  def wasInvoked = invokedCount > 0
  def required: Boolean
}


/** A simple command-line option parsing tool. 
    Example usage:
    <code>
    def main(args:Array[String]): Unit = {
      object opts extends CmdOptions {
        val train = CmdOption("train", "eng.train", "CoNLL-format file from which to get training data.")
        val temperature = CmdOption("temperature", 1.0, "Temperature for the sampler.")
        val iterations = CmdOption("iterations", 15, "Number of iterations of training.")
      }
      opts.parse(args)
      // then later
      for (i <- 0 until opts.iterations.value) ...
    }
    </code>
    @author Andrew McCallum
 */
trait CmdOptions extends scala.collection.Map[String,CmdOption[_]] {
  private val opts = new HashMap[String,CmdOption[_]]
  def get(key:String) = opts.get(key)
  def size = opts.size
  def elements = opts.elements
  def +=[T](c:CmdOption[T]): Unit = {
    if (opts.contains(c.name)) throw new Error("CmdOption "+c.name+" already exists.")
    opts(c.name) = c
  }
  def error(msg:String): Unit = {
    System.err.println(msg)
    System.err.println(usageString)
    System.exit(-1)
  }
  def usageString: String = {
    val sb = new StringBuffer
    sb append "Usage: "
    opts.values.foreach(o => if (o.hasValue) sb.append(o.name+"="+o.valueName) else sb.append(o.name)); sb.append(" ")
    sb.toString
  }
  /** Parse sequence of command-line arguments.  Return sequence of arguments that were unqualified by dashed options. */
  def parse(args:Seq[String]): Seq[String] = {
    val result = new ArrayBuffer[String]
    //opts.values.foreach(o => o.invokedCount = 0) // Reset for each parse?  No, might want to parse multiple times.
    var index = 0
    while (index < args.length) {
      val origIndex = index
      var invoked = false
      val optsIter = opts.values
      while (optsIter.hasNext && !invoked) {
        val opt = optsIter.next
        index = opt.parse(args, index)
        invoked = index != origIndex
        assert(invoked || index == origIndex)
      }
      if (!invoked) {
        // Handle options not recognized by any CmdOption parse
        if (args(index).startsWith("-")) error("Unrecognized option "+args(index))
        result += args(index)
        index += 1
      } 
    }
    this.values.find(o => o.required && o.invokedCount == 0) match {
      case Some(o) => error("Required CmdOption "+o.name+" was not provided.")
      case None =>
    }
    result
  }
	class CmdOption[T](val name:String, val helpMsg:String)(implicit m:Manifest[T]) extends cc.factorie.util.CmdOption[T] {
  	// TODO Add "required" constructor argument when we have Scala 2.8
    def this(name:String, valueName:String, defaultValue:T, helpMsg:String)(implicit m:Manifest[T]) = { 
      this(name, helpMsg)
      this.valueName = valueName
      value = defaultValue
      this.defaultValue = defaultValue
    }
  	def this(name:String, defaultValue:T, helpMsg:String)(implicit m:Manifest[T]) = { 
  		this(name, { val fields = m.erasure.getName.split("[^A-Za-z]+"); if (fields.length > 1) fields.last else fields.first }, defaultValue, helpMsg)
  	}
    def this(name:String, shortName:Char, valueName:String, defaultValue:T, helpMsg:String)(implicit m:Manifest[T]) = { 
      this(name, valueName, defaultValue, helpMsg)
      this.shortName = shortName
    }
    def this(name:String, shortName:Char, defaultValue:T, helpMsg:String)(implicit m:Manifest[T]) = { 
      this(name, defaultValue, helpMsg)
      this.shortName = shortName
    }
    CmdOptions.this += this
  	// TODO When we have Scala 2.8 default args, add a "shortName" one-char alternative here
    var shortName: Char = ' ' // space char indicates no shortName
  	val valueClass: Class[_] = m.erasure
  	var valueName: String = null
  	var defaultValue: T = _
  	var value: T = _
  	var invokedCount = 0
  	def required = false
  	def hasValue = valueClass != classOf[Nothing]
  	/** Attempt to match and process command-line option at position 'index' in 'args'.  
        Return the index of the next position to be processed. */
  	def parse(args:Seq[String], index:Int): Int = {
      if (valueClass == classOf[Nothing] && args(index) == "--"+name || args(index) == "-"+shortName) {
  			// support --help or -h (i.e. no arguments to option)
  			invoke
  			invokedCount += 1
  			index + 1
  		} else if (args(index) == "--"+name || args(index) == "-"+shortName) {
  			// support --file foo, or -f foo (or just --file or -f, in which case value is the defaultValue)
  			var newIndex = index + 1
  			// If the next args does not begin with a "-" assume it is the value of this argument and parse it...
  			if (!args(newIndex).startsWith("-")) newIndex = parseValue(args, newIndex)
  			// ...otherwise the value will just remain the defaultValue
  			invoke
  			invokedCount += 1
  			newIndex
  		} else if (args(index).startsWith("--"+name+"=")) {
  			// support --file=foo
  			val fields = args(index).split("=")
  			if (fields.length != 2) error("Expected a single '=' in command "+args(index))
  			parseValue(List(fields(1)), 0)
  			invoke
  			invokedCount += 1
  			index + 1
  		} else index
  	}
  	/** Called after this CmdOption has been matched and value has been parsed. */
  	def invoke: Unit = {}
  	/** After we have found a match, request that argument(s) to command-line option be parsed. 
        Return the index position that should be processed next. 
        This method allows one option to possibly consume multiple args, (in contrast with parseValue(String).) */
  	protected def parseValue(args:Seq[String], index:Int): Int = { parseValue(args(index)); index + 1 }
  	/** Parse a value from a single arg */
  	protected def parseValue(valueStr:String): Unit = {
  		// TODO Is there a better way to do this
  		if (valueClass eq classOf[Int]) value = Integer.parseInt(valueStr).asInstanceOf[T]
      else if (valueClass eq classOf[Float]) value = java.lang.Float.parseFloat(valueStr).asInstanceOf[T]
      else if (valueClass eq classOf[Double]) value = java.lang.Double.parseDouble(valueStr).asInstanceOf[T]
      else if (valueClass eq classOf[Short]) value = java.lang.Short.parseShort(valueStr).asInstanceOf[T]
      else if (valueClass eq classOf[Long]) value = java.lang.Long.parseLong(valueStr).asInstanceOf[T]
      else if (valueClass eq classOf[Boolean]) value = java.lang.Boolean.parseBoolean(valueStr).asInstanceOf[T]
      else if (valueClass eq classOf[Char]) value = valueStr.apply(0).asInstanceOf[T]
      else if (valueClass eq classOf[String]) value = valueStr.asInstanceOf[T]
      else throw new Error("CmdOption does not handle type "+valueClass.getName)
      // TODO Add an option that will run the interpreter on some code
    }
  	// TODO Format long help messages more nicely.
  	def helpString: String = 
  		if (valueClass != classOf[Nothing]) "--%-15s %s\n".format(name+"="+valueName, helpMsg+"  Default="+defaultValue)
  		else "--%-15s %s\n".format(name, helpMsg)
  }
}

/** Default CmdOption collection that should be included in most CmdOptions. */
trait DefaultCmdOptions extends CmdOptions {
  new CmdOption("help", "Print this help message.") {
    override def invoke = {
      DefaultCmdOptions.this.values.foreach(o => println(o.helpMsg))
      System.exit(0)
    }
  }
  new CmdOption("version", "Print version numbers.") {
    override def invoke = {
      println("FACTORIE version "+cc.factorie.factorie.factorieVersionString)
      // TODO How do I print the Scala and JVM version numbers?
      System.exit(0)
    }
  }
  new CmdOption("config", "FILE", "config.factorie", "Read command option values from a file") {
    override def invoke = {
      import java.io.File
      import scala.io.Source
      val contents = Source.fromFile(this.value).mkString
      val args = contents.split("\\s+")
      DefaultCmdOptions.this.parse(args)
    }
  }
}