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
package cc.factorie.util


/** An object that has a unique identifier of type A that is consistent across serialization. 
    @author Andrew McCallum */
trait TypedUniqueId[+A] {
  def uniqueId: A
}

/** An object that has a unique identifier of type String that is consistent across serialization. 
    @author Andrew McCallum */
trait UniqueId extends TypedUniqueId[String]


