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

package cc.factorie.tutorial
import cc.factorie.directed.{Dirichlet, Discrete, MaximizeDirichletByMomentMatching, _}
import cc.factorie.infer.Maximize
import cc.factorie.variable._

/** Simple demonstration of Dirichlet-distributed proportions generating Discrete values. */
object DirichletDemo {

  def main(args:Array[String]): Unit = {
    implicit val random = new scala.util.Random(0)
    object WordDomain extends EnumDomain { val a, b, c, d, e, f = Value }
    class Word extends DiscreteVariable { def domain = WordDomain }
    implicit val model = DirectedModel()
    
    val masses = new MassesVariable(new DenseMasses1(WordDomain.size, 2.0))
    val p1 = new ProportionsVariable(new DenseProportions1(WordDomain.size))
    p1 :~ Dirichlet(masses)

    val data = for (i <- 0 until 500) yield new Word :~ Discrete(p1)
    MaximizeProportions.infer(Seq(p1), model)
    Maximize(Seq(p1), model)

    val ps = for (i <- 0 until 1000) yield ProportionsVariable.dense(WordDomain.size) :~ Dirichlet(masses)
    MaximizeDirichletByMomentMatching(masses, model)
  }
  
}
