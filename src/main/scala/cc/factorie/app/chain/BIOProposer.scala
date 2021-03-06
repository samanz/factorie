/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

package cc.factorie.app.chain
import cc.factorie._

/** A proposer that enforces BIO constraints 
    @author Mike Wick */
// TODO Needs to be updated to work in this package
/*
abstract class BIOProposer(model:TemplateModel) extends MHSampler[LabelVariable[String]](model)
{
  def labelSpace : Array[String]
  def labels : Seq[LabelVariable[String]] //def labels = this.map(_.label)
  def propose(context:L)(implicit delta:DiffList) : Double = {
    //val labelSpace = Domain[Label]
    val label = labels(random.nextInt(labels.size))
    //var label = labels(indices.get(index))
    var newLabel = labelSpace(random.nextInt(labelSpace.length))
    if (newLabel.startsWith("I-")) {
      val suffix = newLabel.substring(2,newLabel.length)
      if(label.hasPrev && label.prev.categoryValue.indexOf(suffix) == -1)
        label.prev.setCategory("B-"+suffix)(delta)
      if(!label.hasPrev)
        newLabel="B-"+suffix
    }
    if(newLabel.startsWith("B-")) {
      val suffix = newLabel.substring(2,newLabel.length)
      if(label.hasNext && label.next.categoryValue.indexOf("I-") != -1 && label.next.value.indexOf(suffix) == -1) {
        //TODO check if label.next.next isn't violated
        if(random.nextBoolean)
          label.next.setCategory("I-"+suffix)(delta)
        else 
          label.next.setCategory("O")(delta)
      }
    }
    label.setCategory(newLabel)(delta)
    0.0 //TODO calculate this precisely
  }
}
*/

