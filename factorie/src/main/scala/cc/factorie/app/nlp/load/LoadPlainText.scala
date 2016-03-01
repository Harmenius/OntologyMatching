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

package cc.factorie.app.nlp.load
import java.io.File

import cc.factorie.app.nlp._

import scala.util.matching.Regex 

/** Create Documents from plain text files.
    By default create one Document per file.
    To create multiple Documents from one file, set documentSeparator regex.  
    If the regex specifies a group (via parenthesis) then the Document's name will be set to the match of the contents of this first group. */
class LoadPlainText(documentSeparator:Regex = null) extends Load with LoadDirectory {
  def fromSource(source:io.Source): Seq[Document] = {
    val string = source.getLines().mkString("\n")
    if (documentSeparator eq null) Seq(new Document(string))
    else {
      var docStart = 0
      val matchIterator = documentSeparator.findAllIn(string).matchData
      (for (sepMatch <- matchIterator if sepMatch.start != docStart) yield {
        val doc = new Document(string.substring(docStart, sepMatch.start))
        if (sepMatch.group(1) ne null) doc.setName(sepMatch.group(1))
        docStart = sepMatch.end
        doc
      }).toIndexedSeq
    }
  }
  def fromDirectory(dir:File): Seq[Document] = (for (file <- files(dir)) yield fromFile(file)).flatten
  private def files(directory:File): Seq[File] = {
    if (!directory.exists) throw new Error("File "+directory+" does not exist")
    if (directory.isFile) return List(directory)
    val result = new scala.collection.mutable.ArrayBuffer[File]
    for (entry <- directory.listFiles) {
      if (entry.isFile) result += entry
      else if (entry.isDirectory) result ++= files(entry)
    }
    result
  }
}

object LoadPlainText extends LoadPlainText(documentSeparator = null)
