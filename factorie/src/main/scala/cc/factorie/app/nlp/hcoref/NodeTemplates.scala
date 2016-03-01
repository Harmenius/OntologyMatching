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
package cc.factorie.app.nlp.hcoref

import cc.factorie.{Parameters, _}
import cc.factorie.app.strings
import cc.factorie.la.{Tensor, Tensor1}
import cc.factorie.model._
import cc.factorie.util.VectorUtils
import cc.factorie.variable._

import scala.reflect.ClassTag

/**
 * @author John Sullivan
 */
class EntitySizePrior[Vars <: NodeVariables[Vars]](val weight:Double=0.1, val exponent:Double=1.2, val saturation:Double=128)
  extends TupleTemplateWithStatistics3[Node[Vars]#Exists,Node[Vars]#IsRoot,Node[Vars]#MentionCount]{

  def unroll1(exists: Node[Vars]#Exists) = Factor(exists, exists.node.isRootVar, exists.node.mentionCountVar)
  def unroll2(isRoot: Node[Vars]#IsRoot) = Factor(isRoot.node.existsVar, isRoot, isRoot.node.mentionCountVar)
  def unroll3(mentionCount: Node[Vars]#MentionCount) = Factor(mentionCount.node.existsVar, mentionCount.node.isRootVar, mentionCount)

  def score(exists: Node[Vars]#Exists#Value, isRoot: Node[Vars]#IsRoot#Value, mentionCount: Node[Vars]#MentionCount#Value) = if(exists.booleanValue && isRoot.booleanValue) {
    math.min(saturation, math.pow(mentionCount, exponent)) * weight
  } else {
    0.0
  }
}

class SingleBagTemplate[Vars <: NodeVariables[Vars]](initialWeight:Double, getBag:(Vars => BagOfWordsVariable), getScore:(BagOfWordsVariable => Double), val name:String)(implicit ct:ClassTag[Vars], params:Parameters)
extends Template2[Node[Vars]#Exists, Vars]
with DotFamily2[Node[Vars]#Exists, Vars]
with DebuggableTemplate {

  def unroll1(v: Node[Vars]#Exists) = Factor(v, v.node.variables)
  def unroll2(v: Vars) = Factor(v.node.existsVar, v)
  
  override def statistics(exists: Node[Vars]#Exists#Value, vars:Vars) = if(exists.booleanValue) {
    val score = getScore(getBag(vars))
    report(score, t(0))
    Tensor1(score)
  } else {
    report(0.0, t(0))
    Tensor1(0.0)
  }

  private val t = Tensor1(initialWeight)
  val _weights = params.Weights(t)
  
  def weights: Weights = _weights


}

class BagOfWordsEntropy[Vars <: NodeVariables[Vars]](initialWeight:Double, getBag:(Vars => BagOfWordsVariable), bagName:String = "")(implicit ct:ClassTag[Vars], params:Parameters)
  extends SingleBagTemplate[Vars](initialWeight, getBag, {b =>
    val bag = b.value
    var entropy = 0.0
    var n = 0.0
    val l1Norm = bag.l1Norm

    bag.asHashMap.foreach{ case(k,v) =>
      val vNormAbs = math.abs(v/l1Norm)
      entropy -= (vNormAbs)*math.log(vNormAbs)
      n+=1.0
    }
    if(n>1)entropy /= scala.math.log(n) //normalized entropy in [0,1]
    if((-entropy).isNaN) 0.0 else -entropy
    -entropy
  }, "BagOfWordsEntropy: %s".format(bagName))

class RootNodeBagTemplate[Vars <: NodeVariables[Vars]](initialWeight:Double, getBag:(Vars => BagOfWordsVariable), getScore:(BagOfWordsVariable => Double), val name:String)(implicit ct:ClassTag[Vars], params:Parameters)
  extends Template3[Node[Vars]#Exists,Node[Vars]#IsRoot,Vars]
  with DotFamily3[Node[Vars]#Exists,Node[Vars]#IsRoot,Vars]
  with DebuggableTemplate {

  def unroll1(exists: Node[Vars]#Exists) = Factor(exists, exists.node.isRootVar, exists.node.variables)
  def unroll2(isRoot: Node[Vars]#IsRoot) = Factor(isRoot.node.existsVar, isRoot, isRoot.node.variables)
  def unroll3(vars: Vars) = Factor(vars.node.existsVar, vars.node.isRootVar, vars)


  override def statistics(exists: Node[Vars]#Exists#Value, isRoot: Node[Vars]#IsRoot#Value, vars: Vars) = if(exists.booleanValue && isRoot.booleanValue) {
    val score = getScore(getBag(vars))
    report(score, t(0))
    Tensor1(score)
  } else {
    report(0.0, t(0))
    Tensor1(0.0)
  }

  private val t = Tensor1(initialWeight)
  val _weights = params.Weights(t)

  def weights: Weights = _weights

}

class BagOfWordsSizePrior[Vars <: NodeVariables[Vars]](initialWeight:Double, getBag:(Vars => BagOfWordsVariable), bagName:String = "")(implicit ct:ClassTag[Vars], params:Parameters)
  extends RootNodeBagTemplate[Vars](initialWeight, getBag, {bag => if (bag.size > 0) - bag.size.toDouble / bag.value.l1Norm else 0.0}, "BagOfWordsSizePrior: %s".format(bagName))

class EmptyBagPenalty[Vars <: NodeVariables[Vars]](initialWeight:Double, getBag:(Vars => BagOfWordsVariable), bagName:String = "")(implicit ct:ClassTag[Vars], params:Parameters)
  extends RootNodeBagTemplate[Vars](initialWeight, getBag, {bag => if (bag.size == 0) -1.0 else 0.0}, "EmptyBagPenalty: %s".format(bagName))

class EntityNameTemplate[Vars <: NodeVariables[Vars]](val firstLetterWeight:Double=4.0, val fullNameWeight:Double=4.0,val weight:Double=64,val saturation:Double=128.0, val penaltyOnNoName:Double=2.0, getBag:(Vars => BagOfWordsVariable), bagName:String = "")(implicit ct:ClassTag[Vars], params:Parameters)
  extends TupleTemplateWithStatistics3[Node[Vars]#Exists,Node[Vars]#IsRoot,Vars]
  with DebuggableTemplate {

  val name = "EntityNameTemplate: %s".format(bagName)

  def unroll1(exists: Node[Vars]#Exists) = Factor(exists, exists.node.isRootVar, exists.node.variables)
  def unroll2(isRoot: Node[Vars]#IsRoot) = Factor(isRoot.node.existsVar, isRoot, isRoot.node.variables)
  def unroll3(vars: Vars) = Factor(vars.node.existsVar, vars.node.isRootVar, vars)


  override def score(exists: Node[Vars]#Exists#Value, isRoot: Node[Vars]#IsRoot#Value, vars: Vars) = {
    var score = 0.0
    var firstLetterMismatches = 0
    var nameMismatches = 0
    val bag = getBag(vars)
    bag.value.asHashMap.keySet.pairs.foreach { case(tokI, tokJ) =>
      if(tokI.charAt(0) != tokJ.charAt(0)) {
        firstLetterMismatches += 1
      }
      if(tokI.length > 1 && tokJ.length > 1) {
        nameMismatches += tokI editDistance tokJ
      }
    }
    score -= math.min(saturation, firstLetterMismatches * firstLetterWeight)
    score -= math.min(saturation, nameMismatches * fullNameWeight)
    if(bag.size == 0 && isRoot.booleanValue) {
      score -= penaltyOnNoName
    }
    report(score, weight)
    score * weight
  }
}

abstract class ChildParentTemplate[Vars <: NodeVariables[Vars]](val initWeights:Tensor1)(implicit v1:ClassTag[Vars], params:Parameters)
  extends Template3[ArrowVariable[Node[Vars], Node[Vars]], Vars, Vars]
  with DotFamily3[ArrowVariable[Node[Vars], Node[Vars]], Vars, Vars]{
  override def unroll1(v: ArrowVariable[Node[Vars], Node[Vars]]) = Option(v.dst) match { // If the parent-child relationship exists, we generate factors for it
    case Some(dest) => Factor(v, v.src.variables, dest.variables)
    case None => Nil
  }
  def unroll2(v: Vars) = Nil
  def unroll3(v: Vars) = Nil

  val _weights = params.Weights(initWeights)

  def weights: Weights = _weights
}

class ChildParentCosineDistance[Vars <: NodeVariables[Vars]](weight:Double, shift: Double, getBag:(Vars => BagOfWordsVariable), bagName:String = "")(implicit c:ClassTag[Vars], p:Parameters) extends ChildParentTemplate[Vars](Tensor1(weight)) with DebuggableTemplate {
  val name: String = "ChildParentCosineDistance: %s".format(bagName)

  override def statistics(v1: (Node[Vars], Node[Vars]), child: Vars, parent: Vars): Tensor = {
    val childBag = getBag(child)
    val parentBag = getBag(parent)
    val v = childBag.value.cosineSimilarity(parentBag.value, childBag.value) + shift

    report(v, initWeights(0))
    Tensor1(v)
  }
}

class ChildParentPersonNameTemplate[Vars <: NodeVariables[Vars]](weight:Double, shift:Double, getName:(Vars => PersonNameVariable), scoreNames:((PersonName, PersonName) => Double), varName:String = "")(implicit c:ClassTag[Vars], p:Parameters) extends ChildParentTemplate[Vars](Tensor1(weight)) with DebuggableTemplate {
  def name = "ChildParentPersonName: %s".format(varName)

  override def statistics(v1: (Node[Vars], Node[Vars]), child: Vars, parent: Vars) = {
    val childName = getName(child)
    val parentName = getName(parent)
    val v = scoreNames(childName.value, parentName.--(childName)(null).value) // at this point the child bag has been added to the parent and needs to be remove for regular comparison
    report(v, initWeights(0))
    Tensor1(v)
  }
}

/**
 * This feature serves to ensure that certain merges are forbidden. Specifically no two nodes that share the same value
 * in the [[cc.factorie.variable.BagOfWordsVariable]] should be permitted to merge. Together with [[IdentityFactor]] it can create uniquely
 * identifying features.
 */
class ExclusiveConstraintFactor[Vars <: NodeVariables[Vars]](getBag:(Vars => BagOfWordsVariable), bagName:String = "")(implicit ct:ClassTag[Vars])
  extends TupleTemplateWithStatistics3[ArrowVariable[Node[Vars], Node[Vars]], Vars, Vars]
  with DebuggableTemplate {
  val name = "ExclusiveConstraintFactor: %s".format(bagName)

  override def unroll1(v: ArrowVariable[Node[Vars], Node[Vars]]) = Option(v.dst) match { // If the parent-child relationship exists, we generate factors for it
    case Some(dest) => Factor(v, v.src.variables, dest.variables)
    case None => Nil
  }
  def unroll2(v: Vars) = Nil
  def unroll3(v: Vars) = Nil

  def score(v1: (Node[Vars], Node[Vars]), child: Vars, parent: Vars) = {
    val childBag = getBag(child)
    val parentBag = getBag(parent)
    var result = 0.0
    if((childBag.value.asHashMap.keySet & parentBag.--(childBag)(null).value.asHashMap.keySet).nonEmpty) {
      result = -999999.0
    } else {
      result = 0.0
    }
    report(result, 1.0)
    result
  }
}

class MatchConstraint[Vars <: NodeVariables[Vars]](matchScore:Double, matchPenalty:Double, getBag:(Vars => BagOfWordsVariable), bagName:String = "")(implicit ct:ClassTag[Vars], p:Parameters)
  extends ChildParentTemplate[Vars](Tensor1(matchScore, matchPenalty))
  with DebuggableTemplate {
  def name = "Matching Constraint on: %s".format(bagName)

  override def statistics(v1: ArrowVariable[Node[Vars], Node[Vars]]#Value, child: Vars#Value, parent: Vars#Value) = {
    val x = getBag(child)
    val y = (getBag(parent) -- x)(null)
    if(x.value.contains(y.value)) {
      Tensor1(1,0)
    } else {
      Tensor1(0,1)
    }
  }
}

/**
 * This feature serves to account for special information that may uniquely identify an entity. If a merge is proposed
 * between two nodes that share a value in getBag they will be merged. This feature does not ensure that the value in
 * getBag is unique, [[ExclusiveConstraintFactor]] manages that separately.
 */
class IdentityFactor[Vars <: NodeVariables[Vars]](getBag:(Vars => BagOfWordsVariable), bagName:String = "")(implicit ct:ClassTag[Vars])
  extends TupleTemplateWithStatistics3[ArrowVariable[Node[Vars], Node[Vars]], Vars, Vars]
  with DebuggableTemplate {
  val name = "IdentityFactor: %s".format(bagName)

  override def unroll1(v: ArrowVariable[Node[Vars], Node[Vars]]) = Option(v.dst) match { // If the parent-child relationship exists, we generate factors for it
    case Some(dest) => Factor(v, v.src.variables, dest.variables)
    case None => Nil
  }
  def unroll2(v: Vars) = Nil
  def unroll3(v: Vars) = Nil

  def score(v1: (Node[Vars], Node[Vars]), child: Vars, parent: Vars) = {
    val childBag = getBag(child)
    val parentBag = getBag(parent)
    var result = 0.0
    if(childBag.value.asHashMap.exists{case (id, _) => parentBag.value.asHashMap.contains(id)}) {
      result = 9999999.0
    } else {
      result = 0.0
    }
    report(result, 1.0)
    result
  }
}


class ChildParentDistanceFactor[Vars <: NodeVariables[Vars]](weight:Double, shift:Double, getBag:(Vars => BagOfWordsVariable), distance:((BagOfWordsVariable, BagOfWordsVariable) => Double), metricName:String = "", elementName:String = "")(implicit ct:ClassTag[Vars], p:Parameters)
  extends ChildParentTemplate[Vars](Tensor1(weight))
  with DebuggableTemplate {

  val name = "ChildParentDistance: %s-%s".format(metricName, elementName)

  override def statistics(v1: (Node[Vars], Node[Vars]), child: Vars, parent: Vars) = {
    val childBag = getBag(child)
    val parentBag = getBag(parent).--(childBag)(null)
    //println("child: %s parent: %s distance: %.4f".format(childBag, parentBag, distance(childBag, parentBag)))
    Tensor1(distance(childBag, parentBag) + shift)
  }
}

class ChildParentStringDistance[Vars <: NodeVariables[Vars]](weight:Double, shift:Double, getBag:(Vars => BagOfWordsVariable), elementName:String="")(implicit ct:ClassTag[Vars], p:Parameters) extends ChildParentDistanceFactor[Vars](weight, shift, getBag, {(x:BagOfWordsVariable, y:BagOfWordsVariable) => 1 - ( strings.editDistance(x.value.longest,y.value.longest) / math.max(x.value.longest.length, y.value.longest.length))}, "string edit distance", elementName)

class DenseCosineDistance[Vars <: NodeVariables[Vars]](weight:Double, shift:Double, getArray:(Vars => DenseDoubleBagVariable), elementName:String="")(implicit ct:ClassTag[Vars], params:Parameters) extends ChildParentTemplate[Vars](Tensor1(weight)) with DebuggableTemplate {

  val name = "DenseCosineDistance: %s".format(elementName)

  import VectorUtils._
  override def statistics(v1: ArrowVariable[Node[Vars], Node[Vars]]#Value, child: Vars, parent: Vars) = {
    val childArray = getArray(child).value
    val parentArray = getArray(parent).value
    val v = (childArray cosineSimilarityWithParent parentArray)+shift
    if (v.toString == "Infinity"){
      println("got infinite cosine distance from:\n%s\n%s".format(childArray.map(x => "%.4f".format(x)).mkString(","), parentArray.map(x => "%.4f".format(x)).mkString(",")))
    }
    report(v, initWeights(0))
    Tensor1(v)
  }
}

class DenseBagOfWordsEntropy[Vars <: NodeVariables[Vars]](initialWeight:Double, getArray:(Vars => DenseDoubleBagVariable), elementName:String="")(implicit ct:ClassTag[Vars], params:Parameters)
  extends Template2[Node[Vars]#Exists, Vars]
  with DotFamily2[Node[Vars]#Exists, Vars]
  with DebuggableTemplate {

  val name = "DenseBagOfWordsEntropy: %s".format(elementName)

  def unroll1(v: Node[Vars]#Exists) = Factor(v, v.node.variables)
  def unroll2(v: Vars) = Factor(v.node.existsVar, v)

  import VectorUtils._

  override def statistics(exists: Node[Vars]#Exists#Value, vars:Vars) = if(exists.booleanValue) {
    val score = getArray(vars).value.normalizedEntropyForLogValues
    report(score, t(0))
    Tensor1(score)
  } else {
    report(0.0, t(0))
    Tensor1(0.0)
  }

  private val t = Tensor1(initialWeight)
  val _weights = params.Weights(t)
  def weights = _weights

}

