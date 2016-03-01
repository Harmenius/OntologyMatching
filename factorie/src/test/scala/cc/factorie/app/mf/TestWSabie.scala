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
package cc.factorie.app.mf

import cc.factorie._
import cc.factorie.la._
import cc.factorie.optimize.OnlineTrainer
import cc.factorie.variable.DiscreteDomain
import org.junit._

/**
 * User: apassos
 * Date: 4/5/13
 * Time: 3:19 PM
 */
class TestWSabie {
  @Test def simpleTestWsabie() {
    val d = new DiscreteDomain(3)
    val m = new WSabie.WSabieModel(d, 5, new java.util.Random(0))
    val q = new SparseBinaryTensor1(3)
    q += 0
    val p = new SparseBinaryTensor1(3)
    p += 1
    val n = new SparseBinaryTensor1(3)
    n += 2
    val e = new WSabie.WSabieExample(m, q, p, n)
    val trainer = new OnlineTrainer(m.parameters)
    trainer.optimizer.initializeWeights(m.parameters)
    while (!trainer.isConverged) {
      trainer.processExamples(Seq(e))
    }
    assert(m.score(q, p) > m.score(q, n))
  }
}
