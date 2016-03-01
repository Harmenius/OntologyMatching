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
import cc.factorie.app.nlp._

/** The interface common to objects that create Documents from a data source,
    such as plain text files, labeled data from Ontonotes, etc. 
    @author Andrew McCallum */
trait Load {
  // TODO Consider making this Iterator[Document] instead of Seq[Document]; then LoadWikipediaPlainText could inherit this. -akm
  def fromSource(source:io.Source): Seq[Document]
  def fromString(string:String): Seq[Document] = fromSource(io.Source.fromString(string))
  def fromStream(stream:java.io.InputStream, encoding:String = "UTF-8"): Seq[Document] = fromSource(io.Source.fromInputStream(stream, encoding))
  def fromFile(file:java.io.File, encoding:String = "UTF-8"): Seq[Document] = fromSource(io.Source.fromFile(file, encoding))
  def fromFilename(filename:String, encoding:String = "UTF-8"): Seq[Document] = fromFile(new java.io.File(filename), encoding)
}

/** The interface common to objects that create Documents from the files in a directory.
    @author Andrew McCallum */
trait LoadDirectory {
  def fromDirectory(dir:java.io.File): Seq[Document]
}