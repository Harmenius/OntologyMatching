package cc.factorie.optimize

import cc.factorie.la.Tensor
import cc.factorie.model.{WeightsMap, WeightsSet}

// NOTE: these won't work with optimizers that replace the Tensor in the weights, since it needs to be able to mutate things

object ConstraintHelpers {
  def projectl2(t: Tensor, lambda: Double): Unit = {
    val twonorm = t.twoNorm
    if (twonorm > lambda)
      t *= lambda / twonorm
  }
  def projectHypercube(t: Tensor, min: Double, max: Double): Unit = {
    val arr = t.asArray
    val len = arr.length
    var i = 0
    while (i < len) {
      arr(i) = math.min(max, math.max(min, arr(i)))
      i += 1
    }
  }
}

trait L2ConstraintStep extends GradientStep {
  def lambda: Double
  abstract override def doGradStep(weights: WeightsSet, gradient: WeightsMap, value: Double): Unit = {
    super.doGradStep(weights, gradient, value)
    for (k <- gradient.keys) ConstraintHelpers.projectl2(weights(k), lambda)
  }
}
trait L2Constraint extends GradientOptimizer {
  def lambda: Double
  abstract override def step(weights: WeightsSet, gradient: WeightsMap, value: Double): Unit = {
    super.step(weights, gradient, value)
    for (k <- gradient.keys) ConstraintHelpers.projectl2(weights(k), lambda)
  }
}

trait HypercubeConstraintStep extends GradientStep {
  def min: Double
  def max: Double
  abstract override def doGradStep(weights: WeightsSet, gradient: WeightsMap, value: Double): Unit = {
    super.doGradStep(weights, gradient, value)
    for (k <- gradient.keys) ConstraintHelpers.projectHypercube(weights(k), min, max)
  }
}
trait HypercubeConstraint extends GradientOptimizer {
  def min: Double
  def max: Double
  abstract override def step(weights: WeightsSet, gradient: WeightsMap, value: Double): Unit = {
    super.step(weights, gradient, value)
    for (k <- gradient.keys) ConstraintHelpers.projectHypercube(weights(k), min, max)
  }
}