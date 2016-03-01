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
package cc.factorie.app.nlp.segment
import cc.factorie.app.nlp._

import scala.reflect.ClassTag

/** Clean up Token.string according to various standard practices.
    The aim here is to to put into plain text, the way most people would write an email message,
    e.g. un-escaped asterisks, plain quote characters, etc. */
class TokenNormalizer1[A<:TokenString](
    val newTokenString: (Token,String) => A, // potentially a specialized subclass of TokenString, to reflect different choices here.
    val normalizeQuote:Boolean = true, // Convert all double quotes to "
    val normalizeApostrophe:Boolean = true, // Convert all apostrophes to ', even within token strings
    val normalizeCurrency:Boolean = true, // Convert all currency symbols to "$", except cents symbol to "cents"
    val normalizeAmpersand:Boolean = true, // Convert all ampersand symbols (including "&amp;" to "&"
    val normalizeFractions:Boolean = true, // Convert unicode fraction characters to their spelled out analogues, like "3/4"
    val normalizeEllipsis:Boolean = true, // Convert unicode ellipsis character to spelled out analogue, "..."
    val undoPennParens:Boolean = true, // Change -LRB- etc to "(" etc.
    val unescapeSlash:Boolean = true, // Change \/ to /
    val unescapeAsterisk:Boolean = true, // Change \* to *
    val normalizeMDash:Boolean = true, // Convert all em-dashes to double dash --
    val normalizeDash:Boolean = true, // Convert all other dashes to single dash -
    val normalizeHtmlSymbol:Boolean = true, // Convert &lt; to <, etc
    val normalizeHtmlAccent:Boolean = true, // Convert Beyonc&eacute; to Beyonce
    val americanize:Boolean = false
  )(implicit m:ClassTag[A]) extends DocumentAnnotator {
  
  val dashRegex = ("\\A("+DeterministicRegexTokenizer.dash+")+\\Z").r
  val mdashRegex = ("\\A("+DeterministicRegexTokenizer.mdash+")+\\Z").r
  //val quote = "``|''|[\u2018\u2019\u201A\u201B\u201C\u201D\u0091\u0092\u0093\u0094\u201A\u201E\u201F\u2039\u203A\u00AB\u00BB]{1,2}|[`\"\u201C\u201D\\p{Pf}]|$quot;|(?:['\u0092\u2019]|&apos;){1,2}"
  val quoteRegex = ("\\A("+DeterministicRegexTokenizer.quote+")\\Z").r
  val ellipsisRegex = ("\\A("+DeterministicRegexTokenizer.ellipsis+")\\Z").r
  val apostropheRegex =  DeterministicRegexTokenizer.ap2.replace("'", "").r // ("[\u0092\u2019`\u0091\u2018\u201B]|&(apos|rsquo|#00?39|#00?92|#2019);").r // Note, does not include ' because we don't need to substitute for ' -- it is already what we want; but we do include the single back quotes here
  val currencyRegex = ("\\A("+DeterministicRegexTokenizer.currency+")\\Z").r // Responsible for all cases, except "cents"
  val htmlAccentRegex = ("&([aeiouyntlAEIOUYNTL])(?:acute|grave|uml|circ|orn|tilde|ring);").r //DeterministicTokenizer.htmlAccentedLetter can't be used because it doesn't define the parenthesized "group" needed below.
  val htmlSymbolRegex = ("\\A"+DeterministicRegexTokenizer.htmlSymbol+"\\Z").r
  val htmlSymbolMap = new scala.collection.mutable.HashMap[String,String] {
    override def default(s:String) = s
  } ++= List("&lt;" -> "<", "&gt;" -> ">", "&amp;" -> "&", "&copy;" -> "(c)", "&reg;" -> "(r)", "&trade;" -> "(TM)", "&rsquo;" -> "'", "&lsquo;" -> "'") // TODO complete this collection
  
  // TODO Normalize to `` and '' for better PosTag prediction, etc.
  def processToken(token:Token): Unit = {
    val string = token.string
    if      (undoPennParens && string == "-LRB-") token.attr += newTokenString(token, "(")
    else if (undoPennParens && string == "-RRB-") token.attr += newTokenString(token, ")")
    else if (undoPennParens && string == "-LCB-") token.attr += newTokenString(token, "{")
    else if (undoPennParens && string == "-RCB-") token.attr += newTokenString(token, "}")
    else if (undoPennParens && string == "-LSB-") token.attr += newTokenString(token, "[")
    else if (undoPennParens && string == "-RSB-") token.attr += newTokenString(token, "]")
    else if (normalizeFractions && string == "\u00BC") token.attr += newTokenString(token, "1/4")
    else if (normalizeFractions && string == "\u00BD") token.attr += newTokenString(token, "1/2")
    else if (normalizeFractions && string == "\u00BE") token.attr += newTokenString(token, "3/4")
    else if (normalizeFractions && string == "\u2153") token.attr += newTokenString(token, "1/3")
    else if (normalizeFractions && string == "\u2154") token.attr += newTokenString(token, "2/3")
    else if (normalizeEllipsis && ellipsisRegex.findFirstMatchIn(string) != None) token.attr += newTokenString(token, "...")
    else if (normalizeAmpersand && string == "&amp;") token.attr += newTokenString(token, "&")
    else if (normalizeCurrency && string == "\u00A2") token.attr += newTokenString(token, "cents")
    else if (normalizeCurrency && currencyRegex.findPrefixMatchOf(string) != None) token.attr += newTokenString(token, "$")
    else if (unescapeSlash && string.contains("\\/")) token.attr += newTokenString(token, token.string.replace("\\/", "/")) // this is a simple string replace, not regex-based.
    else if (unescapeAsterisk && string == "\\*") token.attr += newTokenString(token, "*")
    else if (unescapeAsterisk && string == "\\*\\*") token.attr += newTokenString(token, "**")
    else if (normalizeMDash && mdashRegex.findFirstMatchIn(string) != None) token.attr += newTokenString(token, "--") // replace all em-dashes with two dashes
    else if (normalizeDash && dashRegex.findPrefixMatchOf(string) != None) token.attr += newTokenString(token, if (token.hasPrecedingWhitespace && token.hasFollowingWhitespace && !token.precedesNewline) "--" else "-") // replace all dash with dash
    else if (normalizeHtmlAccent && htmlAccentRegex.findFirstMatchIn(string) != None) token.attr += newTokenString(token, htmlAccentRegex.replaceSomeIn(string, m => Some(m.group(1)))) // replace &eacute with e
    else if (normalizeHtmlSymbol && htmlSymbolRegex.findPrefixMatchOf(string) != None) token.attr += newTokenString(token, htmlSymbolMap(string)) // replace all dash with dash
    else if (normalizeQuote && quoteRegex.findFirstMatchIn(string) != None) token.attr += newTokenString(token, "\"") // replace all quotes with ".  This must come before normalizeApostrophe
    else if (normalizeApostrophe && apostropheRegex.findFirstMatchIn(string) != None) token.attr += newTokenString(token, apostropheRegex.replaceAllIn(string, "'")) // replace all apostrophes with simple '
    else if (americanize && BritishToAmerican.contains(string)) token.attr += newTokenString(token, BritishToAmerican(string))
  }
  def process(document:Document): Document = {
    document.tokens.foreach(processToken(_))
    document
  }
  override def tokenAnnotationString(token:Token): String = null
  def prereqAttrs: Iterable[Class[_]] = List(classOf[Token])
  def postAttrs: Iterable[Class[_]] = List(m.runtimeClass)
}

class PlainNormalizedTokenString(token:Token, str:String) extends TokenString(token, str)
object PlainTokenNormalizer extends TokenNormalizer1((t:Token, s:String) => new PlainNormalizedTokenString(t,s))

class OntonotesNormalizedTokenString(token:Token, str:String) extends PlainNormalizedTokenString(token, str)
object OntonotesTokenNormalizer extends TokenNormalizer1((t:Token, s:String) => new OntonotesNormalizedTokenString(t,s)) {
  override def processToken(token:Token): Unit = {
    super.processToken(token)
    // TODO Add more normalization here (not yet sure what needed), but keep Lemma issues separate! 
    // coexist -> co-exist
  }
}

object BritishToAmerican extends scala.collection.mutable.HashMap[String,String] {
  this("colour") = "color"
  // TODO Add more, e.g. see http://oxforddictionaries.com/us/words/british-and-american-spelling
}
