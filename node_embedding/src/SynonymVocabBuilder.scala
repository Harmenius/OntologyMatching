import java.lang.reflect.{Field, Method}

import cc.factorie.app.nlp.embeddings.{VocabBuilder, vocab_word}

/**
  * Created by harmen on 4-6-16.
  */
class SynonymVocabBuilder(vocab_hash_size: Int = 20e6.toInt, sampling_table_size: Int = 1e8.toInt, load_factor: Double = 0.7)
  extends VocabBuilder(vocab_hash_size, sampling_table_size, load_factor) {

  // Hack to get private values
  def getPrivateField[A](fieldName: String): A = {
    val fields = this.getClass.getSuperclass.getDeclaredFields
    val trueName = "cc$factorie$app$nlp$embeddings$VocabBuilder$$" + fieldName
    val field = fields.filter(_.getName == trueName).head
    field.setAccessible(true)
    field.get(this).asInstanceOf[A]
  }

  def vocab = getPrivateField[Array[vocab_word]]("vocab")
  val vocab_hash = getPrivateField[Array[Int]]("vocab_hash")

  def addSynonymToVocab(key: String, orig: String): Unit = {
    var orig_ = getId(orig)
    if (orig_ == -1) {
      addWordToVocab(orig)
      orig_ = getId(orig)
    }
    addSynonymToVocab(key, orig_)
  }

  def addSynonymToVocab(key: String, orig: Int): Unit = {
    val id = getId(key)
    if (id == -1) {
      // Add hash that points to original synonym
      var hash = get_word_hash(key)
      while (vocab_hash(hash) != -1) {
        hash = (hash + 1) % vocab_hash_size
      }
      vocab_hash(hash) = orig
    }
    vocab(orig).cn += 1
  }

  def vocab_size(): Int = {
    getPrivateField[Int]("vocab_size")
  }

  def get_word_hash(key: String): Int = {
    val methods = this.getClass.getSuperclass.getDeclaredMethods
    val hashSizeField :Method = methods.filter(_.getName == "cc$factorie$app$nlp$embeddings$VocabBuilder$$get_word_hash").head
    hashSizeField.setAccessible(true)
    hashSizeField.invoke(this, key).asInstanceOf[Int]
  }
}
