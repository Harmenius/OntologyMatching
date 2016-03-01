/* Copyright (C) 2008-2014 University of Massachusetts Amherst.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://github.com/factorie
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */
package cc.factorie.variable

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.ner._
import cc.factorie.app.nlp.segment.DeterministicNormalizingTokenizer
import junit.framework.Assert._
import junit.framework._

class TestSpanVariable extends TestCase with cc.factorie.util.FastLogging {

  class MySpanBuffer extends SpanVarBuffer[TokenSpan,Section,Token]

  def testDiffLists(): Unit = {
     val doc = load.LoadPlainText.fromString("aaa bb John Smith eee ff ggg").head
     val sl = new MySpanBuffer
     doc.attr += sl
       
     DeterministicNormalizingTokenizer.process(doc)
    
     //doc.foreach(logger.debug(_))
     assertEquals(7, doc.tokenCount)
     val d = new DiffList
     val s1 = new TokenSpan(doc.asSection, 1, 1)
     doc.attr[MySpanBuffer].add(s1)(d)
     assert(sl.head.start == 1)
     //logger.debug("DiffList "+d)
     //logger.debug("new span 1 1")
     //logger.debug(doc.spans.mkString("\n"))
     //logger.debug("DiffList "+d)
     d.undo()
     //logger.debug("undo")
     //logger.debug("DiffList "+d)
     //logger.debug(doc.spans.mkString("\n"))
     assert(sl.length == 0)
     val s2 = new ConllNerSpan(doc.asSection, 2, 2, "PER")
     sl += s2
     assert(s2.string == "John Smith")
     val s3 = new TokenSpan(doc.asSection, 4, 1)
     sl += s3
     assert(sl.spansOfClass[NerSpan].length == 1)
     val d2 = new DiffList
     sl.remove(s3)(d2)
     assert(sl.length == 1)
     d2.undo()
     assert(sl.length == 2)
     sl.clear()
     assert(sl.length == 0)
   }

}


object TestSpanVariable extends TestSuite {
  addTestSuite(classOf[TestSpanVariable])
  def main(args: Array[String]) {
    junit.textui.TestRunner.run(this)
  }
}
