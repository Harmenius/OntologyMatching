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

import junit.framework._

/** @author Pallika Kanani */
class TestIntegerVariable extends TestCase with cc.factorie.util.FastLogging {

   def testDiffLists(): Unit = {
      val initialValue:Int = 100
      val v = new IntegerVariable(initialValue)
      val d: DiffList = new DiffList()
      logger.debug("Initial Value = " + v.intValue)
      v.set(200)(d)
      v.set(300)(d)
      v.set(400)(d)
      v.set(500)(d)
      v.set(600)(d)
      d.reverse.foreach( a => a.undo())
      assert(v.intValue == initialValue)
   }
}


object TestIntegerVariable extends TestSuite {
  addTestSuite(classOf[TestIntegerVariable])
  def main(args: Array[String]) {
    junit.textui.TestRunner.run(this)
  }
}
