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
package cc.factorie.optimize

import cc.factorie.la._
import cc.factorie.model.{Weights, WeightsMap, WeightsSet}
import cc.factorie.util.{DenseDoubleSeq, DoubleSeq, RangeIntSeq}

/**
 * Implements the Regularized Dual Averaging algorithm of Xiao with support for Group Lasso,
 * specifically l2-norm group regularization, as derived for dual averaging-based methods in
 * Yang et al., Online Learning for Group Lasso.
 *
 * This does not support l1-regularization (a.k.a. Sparse Group Lasso) because the updates can't
 * be efficiently calculated in the stochastic sparse gradient setting. It also does not yet
 * support l2(-squared)-regularization since this seems a bit redundant with the l2-norm-based
 * group regularization.
 *
 * @param rate The base learning rate
 * @param groupl2Weights Strength of regularization for each group
 * @param numExamples The number of examples for online training, used to scale regularizers
 */
class GroupLassoRDA(val rate: Double = 0.1, val groupl2Weights: Weights => Double = _ => 0.0, numExamples: Int = 1) extends GradientOptimizer {
  var initialized = false

  def step(weights: WeightsSet, gradient: WeightsMap, value: Double) {
    if (!initialized) initializeWeights(weights)
    weights.+=(gradient)
  }
  def initializeWeights(weights: WeightsSet): Unit = {
    if (initialized) return
    for (key <- weights.keys) key.value match {
      case t: Tensor1 => weights(key) = new GroupLassoRDATensor1(t, rate, groupl2Weights(key) / numExamples)
      case t: Tensor2 => weights(key) = new GroupLassoRDATensor2(t, rate, groupl2Weights(key) / numExamples)
      case t: Tensor3 => weights(key) = new GroupLassoRDATensor3(t, rate, groupl2Weights(key) / numExamples)
      case t: Tensor4 => weights(key) = new GroupLassoRDATensor4(t, rate, groupl2Weights(key) / numExamples)
    }
    initialized = true
  } 

  def finalizeWeights(weights: WeightsSet): Unit =
    for (key <- weights.keys) {
      val scaledTensor = key.value
      weights(key) = key.newBlankTensor
      scaledTensor.foreachElement((i, v) => key.value(i) = v)
    }
  def reset(): Unit = initialized = false
  def isConverged = false

  private trait GroupLassoRDATensor extends Tensor with DenseDoubleSeq {
    def activeDomainSize = activeDomain.length
    def forallActiveElements(f: (Int, Double) => Boolean) = forallElements(f)

    def activeDomain = new RangeIntSeq(0, length)
    val gradients: Array[Double]
    var t = 0
    val rate: Double
    val groupl2: Double

    var cachedGradientsTwoNormSquared: Double = 0.0

    // can we use += here?
    def copyToDense[D <: DenseTensor](d: D): D = {
      var i = 0
      assert(length == d.length)
      while (i < length) {
        d(i) = apply(i)
        i += 1
      }
      d
    }

    override def update(i: Int, v: Double): Unit = sys.error("GroupLassoRDATensor can't be updated")
    override def apply(i: Int): Double = {
      // w_t = -1.0/(gamma*sqrt(t)) * [ 1 - t*lambda*sqrt(d_g) / ||v_t||_2 ]_+ * v_t
      // cf. equation (10) of Yang et al.
      val norm = math.sqrt(math.max(0.0, cachedGradientsTwoNormSquared))
      if (norm == 0.0)
        0.0
      else {
        val scale = -1.0 / (rate * math.sqrt(t))
        val shrinkage = math.max(0.0, 1.0 - t * groupl2 * math.sqrt(size) / norm)
        val w_t = scale * shrinkage * gradients(i)
        w_t
      }
    }

    override def +=(ds: DoubleSeq, factor: Double) {
      t += 1
      ds match {
        case o: SparseTensor =>
          val len = o.activeDomainSize
          val indices = o._indices
          val values = o._valuesSeq
          var i = 0
          while (i < len) {
            val idx = indices(i)
            cachedGradientsTwoNormSquared -= gradients(idx) * gradients(idx)
            gradients(idx) += values(i) * factor
            cachedGradientsTwoNormSquared += gradients(idx) * gradients(idx) * factor * factor
            i += 1
          }
        case o: DenseTensor =>
          val arr = o.asArray
          var i = 0
          while (i < arr.length) {
            cachedGradientsTwoNormSquared -= gradients(i) * gradients(i)
            gradients(i) += arr(i) * factor
            cachedGradientsTwoNormSquared += gradients(i) * gradients(i) * factor * factor
            i += 1
          }
        case _ => sys.error("no match statement for " + ds.getClass.getName)
      }
    }

    override def dot(ds: DoubleSeq) = {
      var res = 0.0
      ds.foreachActiveElement((i, x) => res += apply(i) * x)
      res
    }
    def copy: Tensor = sys.error("Method copy not defined on class " + getClass.getName)
    def blankCopy: Tensor = sys.error("Method blankCopy not defined on class " + getClass.getName)
    def +=(i: Int, v: Double): Unit = sys.error("You should add tensors all at once to the GroupLassoRDATensor")
    def zero(): Unit = {
      for (i <- 0 until length) gradients(i) = 0
      cachedGradientsTwoNormSquared = 0.0
    }
  }

  private class GroupLassoRDATensor1(baseTensor: Tensor1, val rate: Double, val groupl2: Double) extends GroupLassoRDATensor with Tensor1 {
    val gradients = baseTensor.asArray
    val dim1 = baseTensor.dim1
    def isDense = false
    override def copy = copyToDense(new DenseTensor1(dim1))
  }
  private class GroupLassoRDATensor2(baseTensor: Tensor2, val rate: Double, val groupl2: Double) extends GroupLassoRDATensor with Tensor2 {
    val gradients = baseTensor.asArray
    val dim1 = baseTensor.dim1
    val dim2 = baseTensor.dim2
    def activeDomain1 = new RangeIntSeq(0, dim1)
    def activeDomain2 = new RangeIntSeq(0, dim2)
    def isDense = false
    override def copy = copyToDense(new DenseTensor2(dim1, dim2))
  }
  private class GroupLassoRDATensor3(baseTensor: Tensor3, val rate: Double, val groupl2: Double) extends GroupLassoRDATensor with Tensor3 {
    val gradients = baseTensor.asArray
    val dim1 = baseTensor.dim1
    val dim2 = baseTensor.dim2
    val dim3 = baseTensor.dim3
    def isDense = false
    def activeDomain1 = new RangeIntSeq(0, dim1)
    def activeDomain2 = new RangeIntSeq(0, dim2)
    def activeDomain3 = new RangeIntSeq(0, dim3)
    override def copy = copyToDense(new DenseTensor3(dim1, dim2, dim3))
  }
  private class GroupLassoRDATensor4(baseTensor: Tensor4, val rate: Double, val groupl2: Double) extends GroupLassoRDATensor with Tensor4 {
    val gradients = baseTensor.asArray
    val dim1 = baseTensor.dim1
    val dim2 = baseTensor.dim2
    val dim3 = baseTensor.dim3
    val dim4 = baseTensor.dim4
    def isDense = false
    def activeDomain1 = new RangeIntSeq(0, dim1)
    def activeDomain2 = new RangeIntSeq(0, dim2)
    def activeDomain3 = new RangeIntSeq(0, dim3)
    def activeDomain4 = new RangeIntSeq(0, dim4)
    override def copy = copyToDense(new DenseTensor4(dim1, dim2, dim3, dim4))
  }
}
