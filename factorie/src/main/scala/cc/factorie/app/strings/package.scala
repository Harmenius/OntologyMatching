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

package cc.factorie.app

package object strings {

  /** Read the entire contents of the InputStream with the given encoding, and return them as a String. */
  def inputStreamToString(is:java.io.InputStream, encoding:String = "UTF-8"): String = {
    readerToString(new java.io.InputStreamReader(is, encoding))
  }

  /** Read the entire contents of the Reader and return them as a String. */
  def readerToString(reader:java.io.Reader): String = {
    val buffer = new Array[Char](0x10000)
    val out = new StringBuilder()
    var read = 0
    do {
      read = reader.read(buffer, 0, buffer.length)
      if (read > 0)
        out.appendAll(buffer, 0, read)
    } while (read >= 0)
    out.toString()
  }

  /** Return a string that captures the generic "shape" of the original word, 
      mapping lowercase alphabetics to 'a', uppercase to 'A', digits to '1', whitespace to ' '.
      Skip more than 'maxRepetitions' of the same character class. */
  def stringShape(word:String, maxRepetitions:Int): String = {
    val sb = new StringBuffer
    var i = 0; var c = 'x'; var prevc = 'x'; var repetitions = 0
    while (i < word.length) {
      val char = word(i)
      if (Character.isUpperCase(char)) c = 'A'
      else if (Character.isLowerCase(char)) c = 'a'
      else if (Character.isDigit(char)) c = '1'
      else if (Character.isWhitespace(char)) c = ' '
      else c = char
      if (c == prevc) repetitions += 1
      else { prevc = c; repetitions = 0 }
      if (repetitions < maxRepetitions) sb.append(c)
      i += 1
    }
    sb.toString
  }
  
  /** Return Strings representing all possible character sub-sequences of length between "min" and "max", with prepended "<" and appended ">" to indicate start and end of the input string. */
  def charNGrams(word:String, min:Int, max:Int): Seq[String] = {
    val w = "<"+word+">"
    val prefixes = for (e <- min+1 to math.min(max+1, word.length)) yield w.substring(0, e)
    val suffices = for (b <- math.max(w.length-1-max, 0) to w.length-1-min) yield w.substring(b, w.length)
    prefixes ++ suffices
    //for (i <- 0 until w.length; j <- min to max; if (i+j < w.length)) yield w.substring(i,i+j)
  }
  def prefix(word:String, length:Int): String = word.substring(0, math.min(length, word.length))
  def suffix(word:String, length:Int): String = { val l = word.length; word.substring(math.max(0, l-length), l) }
  
  // Simplified form of word for feature generation
  val recentYearRegex = "(19|20)\\d\\d".r
  val digitsRegex = "\\d+".r
  val containsDigitRegex = ".*\\d.*".r
  /** Return input string, with digits replaced, either the whole string with "<YEAR>" or "<NUM>" or just the digits replaced with "#" */
  def simplifyDigits(word:String): String = {
    if (recentYearRegex.findFirstIn(word).nonEmpty) "<YEAR>"
    else if (digitsRegex.findFirstIn(word).nonEmpty) "<NUM>"
    else if (containsDigitRegex.findFirstIn(word).nonEmpty) word.replaceAll("\\d","#")
    else word
  }
  def collapseDigits(word:String): String = {
    if (cc.factorie.app.nlp.lexicon.NumberWords.containsWord(word) || containsDigitRegex.findFirstIn(word).nonEmpty) "0" else word
  }
  def replaceDigits(word:String): String = {
    if (cc.factorie.app.nlp.lexicon.NumberWords.containsWord(word)) "<NUM>" else digitsRegex.replaceAllIn(word, "0")
  }

  /** Implements Levenshtein Distance, with specific operation costs to go from this String to String s2. */
  def editDistance(s:String, s2: String, substCost: Int = 1, deleteCost: Int = 1, insertCost: Int = 1): Int = {
    if (s.length == 0) s2.length
    else if (s2.length == 0) s.length
    else {
      val d = Array.ofDim[Int](s.length + 1, s2.length + 1)
      for (i <- 0 to s.length)
        d(i)(0) = i * deleteCost
      for (i <- 0 to s2.length)
        d(0)(i) = i * insertCost
      for (i <- 1 to s.length; j <- 1 to s2.length) {
        val cost = if (s(i - 1) == s2(j - 1)) 0 else substCost
        d(i)(j) = math.min(d(i - 1)(j) + deleteCost, math.min(d(i)(j - 1) + insertCost, d(i - 1)(j - 1) + cost))
      }
      d(s.length)(s2.length)
    }
  }

  def porterStem(s:String): String = PorterStemmer(s)
  
}

package object chineseStrings {

  def isEndOfSentence(character: Char): Boolean = {

    List(
      0x002C,
      0x3002,
      0xFE50,
      0xFE52,
      0xFE54,
      0xFE56,
      0xFE57,
      0xFF01,
      0xFF0C,
      0xFF1B,
      0xFF1F,
      0xFF61
    ).exists(
        punct => character == punct
    )
  }

  def isWhiteSpace(character: Char): Boolean = {

    List(
      (0x0000, 0x0020),
      (0x0085, 0x0085),
      (0x2000, 0x200F),
      (0x2028, 0x202F),
      (0x205F, 0x206F),
      (0x3000, 0x3000)
    ).exists(
        range => character >= range._1 && character <= range._2
    )
  }

  def isPunctuation(character: Char): Boolean = {
    List(
      (0x0021, 0x002F),
      (0x003A, 0x0040),
      (0x005B, 0x0060),
      (0x007B, 0x007E),
      (0x2000, 0x206F),
      (0x2E00, 0x2E42),
      (0x3000, 0x303F),
      (0xFE10, 0xFE19),
      (0xFE30, 0xFE42),
      (0xFE50, 0xFE6F),
      (0xFF01, 0xFF0F),
      (0xFF1A, 0xFF20),
      (0xFF3B, 0xFF40),
      (0xFF5B, 0xFF65)
    ).exists(
      range => character >= range._1 && character <= range._2
    )
  }

  def hasPunctuation(word: String): Boolean = {
    word.exists( character => isPunctuation(character) )
  }

  def isNumeric(character: Char): Boolean = {
    List(
      (0x0030, 0x0039),
      (0xFF10, 0xFF19)
    ).exists(
      range => character >= range._1 && character <= range._2
    )
  }

  def hasNumeric(word: String): Boolean = {
    word.exists( character => isNumeric(character) )
  }

  def isChineseNumeric(character: Char): Boolean = {
    List(
      (0x4E00, 0x4E00),
      (0x4E8C, 0x4E8C),
      (0x4E09, 0x4E09),
      (0x56DB, 0x56DB),
      (0x4E94, 0x4E94),
      (0x561D, 0x561D),
      (0x4E03, 0x4E03),
      (0x516B, 0x516B),
      (0x4E5D, 0x4E5D),
      (0x5341, 0x5341),
      (0x767E, 0x767E),
      (0x5343, 0x5343),
      (0x4E07, 0x4E07),
      (0x842C, 0x842C),
      (0x5104, 0x5104),
      (0x4EBF, 0x4EBF),
      (0x5146, 0x5146)
    ).exists(
      range => character >= range._1 && character <= range._2
    )
  }

  def hasChineseNumeric(word: String): Boolean = {
    word.exists( character => isChineseNumeric(character) )
  }

  def isAlpha(character: Char): Boolean = {
    List(
      (0x0041, 0x005A),
      (0x0061, 0x007A),
      (0xFF21, 0xFF3A),
      (0xFF41, 0xFF5A)
    ).exists(
      range => character >= range._1 && character <= range._2
    )
  }

  def hasAlpha(word: String): Boolean = {
    word.exists( character => isAlpha(character) )
  }
}