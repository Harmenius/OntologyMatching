import java.util.Random

import cc.factorie.app.nlp.embeddings.VocabBuilder
import org.apache.commons.lang.NotImplementedException

/**
  * Created by harmen on 3-6-16.
  */
class MultiVocabBuilder(n: Int = 2) extends VocabBuilder {

  val rand: Random = new Random(4)
  lazy val vocabs: List[VocabBuilder] = List.fill(n)(new VocabBuilder())

  override def getRandWordId(): Int = {
    val N = size()
    rand.nextInt(N)
  }

  override def getRandWord(): String = {
    val i = getRandWordId()
    getWord(i)
  }

  override def getWord(id: Int) : String = {
    var cur_N = 0
    for (v <- vocabs) {
      if (cur_N + v.size >= id) {
        return v.getWord(id - cur_N)
      } else {
        cur_N += v.size
      }
    }
    "ID not in vocab"
  }

  override def getId(word: String): Int = {
    var w = -1
    for (v <- vocabs) {
      w = v.getId(word)
      if (w != -1)
        return w
    }
    w
  }

  override def sortVocab(min_count: Int = 5, ignoreStopWords: Int = 0, max_vocab_size: Int = 2e6.toInt) : Unit = {
    for (v <- vocabs)
      v.sortVocab(min_count, ignoreStopWords, max_vocab_size)
  }

  override def saveVocab(filename: String, binary: Int = 0, encoding: String = "UTF8") : Unit = {
    val basename = filename.split(".").dropRight(1).mkString(".")
    val extension = "." + filename.split(".").last
    for ((v, i) <- vocabs.zipWithIndex) {
      val fn = "%s_%d%s".format(basename, i, extension)
      v.saveVocab(fn, binary, encoding)
    }
  }

  override def loadVocab(filename: String, encoding: String = "UTF8") : Unit = {
    val basename = filename.split(".").dropRight(1).mkString(".")
    val extension = "." + filename.split(".").last
    for ((v, i) <- vocabs.zipWithIndex) {
      val fn = "%s_%d%s".format(basename, i, extension)
      v.loadVocab(fn, encoding)
    }
  }

  override def buildSamplingTable() : Unit = {
    for (v <- vocabs)
      v.buildSamplingTable()
  }

  override def buildSubSamplingTable(s: Double) : Unit = {
  }

  override def getSubSampleProb(id: Int): Double = {
    throw new NotImplementedException("Author did not require this method.")
  }

  override def size(): Int = {
    vocabs.map(v => v.size()).sum
  }

  override def trainWords(): Long = {
    vocabs.map(v => v.trainWords()).sum
  }

  override def getCount(id: Int): Int = {
    var cur_N = 0
    for (v <- vocabs) {
      if (cur_N + v.size >= id) {
        return v.getCount(id - cur_N)
      } else {
        cur_N += v.size
      }
    }
    -1
  }

  override def getCount(word: String): Int = {
    getCount(getId(word))
  }

  def addWordToVocab(key: String, vid: Int): Unit = {
    vocabs(vid).addWordToVocab(key)
  }

  def getVocab(i: Int) : VocabBuilder = vocabs(i)
}
