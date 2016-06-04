import cc.factorie.app.nlp.embeddings.{VocabBuilder, vocab_word}

/**
  * Created by harmen on 4-6-16.
  */
class SynonymVocabBuilder extends VocabBuilder {

  // Hack to get private values
  val vocabField = super.getClass.getDeclaredField("vocab")
  vocabField.setAccessible(true)
  val vocab: Array[vocab_word] = vocabField.get(this).asInstanceOf[Array[vocab_word]]

  val hashField = super.getClass.getDeclaredField("vocab_hash")
  vocabField.setAccessible(true)
  val vocab_hash: Array[Int] = vocabField.get(this).asInstanceOf[Array[Int]]

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
    vocab(id).cn += 1
  }

  def vocab_size: Int = {
    val sizeField = super.getClass.getDeclaredField("vocab_size")
    sizeField.setAccessible(true)
    sizeField.get(this).asInstanceOf[Int]
  }

  def get_word_hash(key: String): Int = {
    val hashSizeField = super.getClass.getDeclaredMethod("get_word_hash")
    hashSizeField.setAccessible(true)
    hashSizeField.invoke(key).asInstanceOf[Int]
  }

  def vocab_hash_size: Int = {
    val sizeField = super.getClass.getDeclaredField("vocab_hash_size")
    sizeField.setAccessible(true)
    sizeField.get(this).asInstanceOf[Int]
  }
}
