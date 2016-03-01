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
package cc.factorie.app.nlp

import cc.factorie.util.FastLogging

import scala.reflect.ClassTag

/**User: apassos
 * Date: 8/7/13
 * Time: 2:48 PM
 */

/** A sequence of DocumentAnnotators packaged as a single DocumentAnnotator.
    This class also properly populates the Document.annotators with a record of which DocumentAnnotator classes provided which annotation classes. */
class DocumentAnnotationPipeline(val annotators: Seq[DocumentAnnotator], val prereqAttrs: Seq[Class[_]] = Seq()) extends DocumentAnnotator {
  var profile = false
  var tokensProcessed = 0
  var msProcessed = 0L
  val timePerAnnotator = collection.mutable.LinkedHashMap[DocumentAnnotator,Long]()
  def postAttrs = annotators.flatMap(_.postAttrs).distinct
  def process(document: Document) = {
    var doc = document
    val t00 = System.currentTimeMillis()
    for (annotator <- annotators; if annotator.postAttrs.forall(!doc.hasAnnotation(_))) {
      val t0 = System.currentTimeMillis()
      doc = annotator.process(doc)
      if (profile) timePerAnnotator(annotator) = timePerAnnotator.getOrElse(annotator, 0L) + System.currentTimeMillis() - t0
      annotator.postAttrs.foreach(a => document.annotators(a) = annotator.getClass)
    }
    if (profile) {
      msProcessed += System.currentTimeMillis() - t00
      tokensProcessed += doc.tokenCount
    }
    doc
  }
  def profileReport: String = {
    s"Processed $tokensProcessed tokens in ${msProcessed/1000.0} seconds, at ${tokensProcessed.toDouble*1000.0/msProcessed} tokens / second " +
    "Speeds of individual components:\n" + timePerAnnotator.map(i => f"   ${i._1.getClass.getSimpleName}%30s: ${tokensProcessed.toDouble*1000.0/i._2}%4.4f tokens/sec ").mkString("\n")
  }
  def tokenAnnotationString(token: Token): String = annotators.map(_.tokenAnnotationString(token)).mkString("\t")
}

/** A Map from annotation class to DocumentAnnotator that provides that annotation. 
    Used to store default ways of getting certain prerequisite annotations. */
class MutableDocumentAnnotatorMap extends collection.mutable.LinkedHashMap[Class[_], () => DocumentAnnotator] {
  def +=(annotator: DocumentAnnotator) = annotator.postAttrs.foreach(a => this(a) = () => annotator)
}

/** A factory for creating DocumentAnnotatorPipelines given requirements about which annotations or which DocumentAnnotators are desired. */
object DocumentAnnotatorPipeline extends FastLogging  {
  val defaultDocumentAnnotationMap: DocumentAnnotatorMap = new collection.immutable.ListMap ++ Seq(
    // Note that order matters here
    classOf[pos.PennPosTag] -> (() => pos.OntonotesForwardPosTagger),
    classOf[parse.ParseTree] -> (() => parse.OntonotesTransitionBasedParser),
    classOf[segment.PlainNormalizedTokenString] -> (() => segment.PlainTokenNormalizer),
    classOf[Token] -> (() => segment.DeterministicNormalizingTokenizer),
    classOf[Sentence] -> (() => segment.DeterministicSentenceSegmenter),
    classOf[lemma.WordNetTokenLemma] -> (() => lemma.WordNetLemmatizer),
    classOf[lemma.SimplifyDigitsTokenLemma] -> (() => lemma.SimplifyDigitsLemmatizer),
    classOf[lemma.CollapseDigitsTokenLemma] -> (() => lemma.CollapseDigitsLemmatizer),
    classOf[lemma.PorterTokenLemma] -> (() => lemma.PorterLemmatizer),
    classOf[lemma.LowercaseTokenLemma] -> (() => lemma.LowercaseLemmatizer),
    classOf[ner.NerTag] -> (() => ner.ConllChainNer), // TODO Should there be a different default?
    classOf[ner.BilouConllNerTag] -> (() => ner.NoEmbeddingsConllStackedChainNer),
    classOf[ner.BilouOntonotesNerTag] -> (() => ner.NoEmbeddingsOntonotesStackedChainNer),
    classOf[ner.ConllNerSpanBuffer] -> (() => ner.BilouConllNerChunkAnnotator),
    classOf[ner.OntonotesNerSpanBuffer] -> (() => ner.BilouOntonotesNerChunkAnnotator),
    //classOf[coref.mention.NerMentionList] -> (() => coref.mention.NerAndPronounMentionFinder),
    //classOf[phrase.GenderLabel[coref.Mention]] -> (() => phrase.GenderLabeler[]),
    classOf[phrase.Gender] -> (() => phrase.MentionPhraseGenderLabeler),
    classOf[phrase.Number] -> (() => phrase.MentionPhraseNumberLabeler),
    classOf[phrase.DatePhraseList] -> (() => phrase.DatePhraseFinder),
    classOf[coref.WithinDocCoref] -> (() => coref.NerForwardCoref),
    classOf[relation.RelationMentionSeq] -> (() => relation.ConllPatternBasedRelationFinder)
    //classOf[phrase.NumberLabel[phrase.NounPhrase]] -> (() => phrase.NounPhraseNumberLabeler),
    //classOf[MentionEntityType] ->  (() => coref.mention.MentionEntityTypeLabeler),
    //classOf[cc.factorie.util.coref.GenericEntityMap[coref.mention.Mention]] -> (() => coref.NerForwardCoref)

  )

  //def apply(goal: Class[_]): DocumentAnnotationPipeline = apply(Seq(goal), defaultDocumentAnnotationMap)
  def apply[A](implicit m:ClassTag[A]): DocumentAnnotationPipeline = apply(defaultDocumentAnnotationMap, Nil, Seq(m.runtimeClass))
  def apply[A,B](implicit m1:ClassTag[A], m2:ClassTag[B]): DocumentAnnotationPipeline = apply(defaultDocumentAnnotationMap, Nil, Seq(m1.runtimeClass, m2.runtimeClass))
  def apply[A,B,C](implicit m1:ClassTag[A], m2:ClassTag[B], m3:ClassTag[C]): DocumentAnnotationPipeline = apply(defaultDocumentAnnotationMap, Nil, Seq(m1.runtimeClass, m2.runtimeClass, m3.runtimeClass))
  def apply[A,B,C,D](implicit m1:ClassTag[A], m2:ClassTag[B], m3:ClassTag[C], m4:ClassTag[D]): DocumentAnnotationPipeline = apply(defaultDocumentAnnotationMap, Nil, Seq(m1.runtimeClass, m2.runtimeClass, m3.runtimeClass, m4.runtimeClass))
  //def apply(goal: Class[_], map: DocumentAnnotatorMap): DocumentAnnotationPipeline = apply(Seq(goal), map)
  def apply[A](map: DocumentAnnotatorMap)(implicit m:ClassTag[A]): DocumentAnnotationPipeline = apply(map, Nil, Seq(m.runtimeClass))
  def apply[A,B](map: DocumentAnnotatorMap)(implicit m1:ClassTag[A], m2:ClassTag[B]): DocumentAnnotationPipeline = apply(map, Nil, Seq(m1.runtimeClass, m2.runtimeClass))
  def apply[A,B,C](map: DocumentAnnotatorMap)(implicit m1:ClassTag[A], m2:ClassTag[B], m3:ClassTag[C]): DocumentAnnotationPipeline = apply(map, Nil, Seq(m1.runtimeClass, m2.runtimeClass, m3.runtimeClass))
  def apply[A,B,C,D](map: DocumentAnnotatorMap)(implicit m1:ClassTag[A], m2:ClassTag[B], m3:ClassTag[C], m4:ClassTag[D]): DocumentAnnotationPipeline = apply(map, Nil, Seq(m1.runtimeClass, m2.runtimeClass, m3.runtimeClass, m4.runtimeClass))

  //def apply(goals:Class[_]*): DocumentAnnotationPipeline = apply(defaultDocumentAnnotationMap, Nil, goals:_*)
  //def apply(prereqs: Seq[Class[_]], goals:Class[_]*): DocumentAnnotationPipeline = apply(defaultDocumentAnnotationMap, prereqs, goals)
  //def forGoals(map:DocumentAnnotatorMap, goals:Class[_]*): DocumentAnnotationPipeline = forGoals(map, Nil, goals)
  def apply(map:DocumentAnnotatorMap, prereqs:Seq[Class[_]], goals:Iterable[Class[_]]): DocumentAnnotationPipeline = {
    val pipeSet = collection.mutable.LinkedHashSet[DocumentAnnotator]()
    val preSet = new scala.collection.mutable.HashSet[Class[_]] ++= prereqs
    def recursiveSatisfyPrereqs(goal: Class[_]) {
      if (!preSet.contains(goal) && (!preSet.exists(x => goal.isAssignableFrom(x)))) {
        val provider = if (map.contains(goal)) map(goal)() else {
          val list = map.keys.filter(k => goal.isAssignableFrom(k))
          assert(list.nonEmpty, s"Could not find annotator for goal $goal, map includes ${map.keys.mkString(", ")}")
          map(list.head)()
        }
        if (!pipeSet.contains(provider)) {
          provider.prereqAttrs.foreach(recursiveSatisfyPrereqs)
          provider.postAttrs.foreach(preSet += _)
          pipeSet += provider
        }
      }
    }
    goals.foreach(recursiveSatisfyPrereqs)
    checkPipeline(pipeSet.toSeq)
    new DocumentAnnotationPipeline(pipeSet.toSeq)
  }

  def apply(annotators:DocumentAnnotator*): DocumentAnnotationPipeline = apply(defaultDocumentAnnotationMap, Nil, annotators:_*)
  def apply(map:DocumentAnnotatorMap, annotators:DocumentAnnotator*): DocumentAnnotationPipeline = apply(map, Nil, annotators:_*)
  def apply(map:DocumentAnnotatorMap, prereqs:Seq[Class[_]], annotators: DocumentAnnotator*): DocumentAnnotationPipeline = {
    val other = new MutableDocumentAnnotatorMap
    map.foreach(k => other += k)
    annotators.foreach(a => other += a) // By being added later, these annotators will overwrite the default ones when there is an overlap
    apply(map=other, prereqs, annotators.flatMap(_.postAttrs))
  }

  def checkPipeline(pipeline: Seq[DocumentAnnotator]) {
    if (logger.level == cc.factorie.util.Logger.DEBUG) {
      logger.debug("-- printing pipeline --")
      for (annotator <- pipeline) {
        logger.debug(s"Annotator ${annotator.getClass.getName} Prereqs(${annotator.prereqAttrs.map(_.getName).mkString(", ")}}) PostAttrs(${annotator.postAttrs.map(_.getName).mkString(", ")})")
      }
    }
    val satisfiedSet = collection.mutable.HashSet[Class[_]]()
    for (annotator <- pipeline) {
      for (requirement <- annotator.prereqAttrs
           if !satisfiedSet.contains(requirement)
           if !satisfiedSet.exists(c => requirement.isAssignableFrom(c)))
        assert(1 == 0, s"Prerequisite $requirement not satisfied before $annotator gets called in pipeline ${pipeline.mkString(" ")}")
      for (provision <- annotator.postAttrs) {
        assert(!satisfiedSet.contains(provision), s"Pipeline attempting to provide $provision twice. Pipeline: ${pipeline.mkString(" ")}")
        satisfiedSet += provision
      }
    }
  }
}

