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
package cc.factorie.app.regress

import cc.factorie.la._
import cc.factorie.optimize.OptimizableObjectives
import cc.factorie.variable.{DiffList, TensorVariable}
import org.junit.Assert._
import org.junit.Test

/**
 * @author apassos
 * @since 9/15/12
 */

class MyTensorVariable(x0: Double, x1: Double, y: Double)(implicit d: DiffList = null) extends TensorVariable[Tensor1] {
  set(new DenseTensor1(1))
  value(0) = y

  val inner = new TensorVariable[Tensor1]
  inner.set(new DenseTensor1(2))
  inner(0) = x0
  inner(1) = x1

  def getFeatures = inner
}

class TestRegression {
  @Test def testSimpleRegression() {
    // y = 2*x0 + x1
    val y0 = new MyTensorVariable(1, 2, 4)
    val y1 = new MyTensorVariable(2, 1, 5)
    val y2 = new MyTensorVariable(1, 1, 3)

    val regressor = LinearRegressionTrainer.train[TensorVariable[Tensor1], MyTensorVariable](Seq(y0, y1, y2), f => f.getFeatures, 0.0)
    assertEquals(4, regressor.regress(y0).dependantValue(0), 0.01)
    assertEquals(5, regressor.regress(y1).dependantValue(0), 0.01)
    assertEquals(3, regressor.regress(y2).dependantValue(0), 0.01)

    val regressor2 = LinearRegressionTrainer.train[TensorVariable[Tensor1], MyTensorVariable](Seq(y0, y1, y2), f => f.getFeatures, 0.0, OptimizableObjectives.epsilonInsensitiveSqMultivariate(0.001))
    assertEquals(4, regressor2.regress(y0).dependantValue(0), 0.01)
    assertEquals(5, regressor2.regress(y1).dependantValue(0), 0.01)
    assertEquals(3, regressor2.regress(y2).dependantValue(0), 0.01)
  }
}

class TestLinearRegressor {

  class MyTensorVariable(x0: Double, x1: Double, y: Double)(implicit d: DiffList = null) extends TensorVariable[Tensor1] {
    // the target value
    set(new DenseTensor1(Array(y)))
    // the dependent values
    val features = new TensorVariable[Tensor1](new DenseTensor1(Array(x0, x1)))
  }

  @Test
  def testLinearRegressor {
    // y = 2*x_0 + x_1
    val weights = new DenseTensor2(Array(Array(2.0), Array(1.0)))
    val r = new LinearRegressor[TensorVariable[Tensor1], MyTensorVariable](v=>v.features, weights)

    // y = 2*3 + 1*4 = 10
    val result = r.regress(new MyTensorVariable(3, 4, 0))
    assertEquals(10, result.dependantValue(0), 0.01)

    // perform multiple regressions
    val results = r.regress(Seq(new MyTensorVariable(3,4,0), new MyTensorVariable(5,6,0)))
    assertEquals(10, results(0).dependantValue(0), 0.01)
    assertEquals(16, results(1).dependantValue(0), 0.01)

  }
}
