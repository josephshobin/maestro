//   Copyright 2014 Commonwealth Bank of Australia
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

package au.com.cba.omnia.maestro.core
package upload

import java.io.File

import scala.util.matching.Regex

import scalaz._, Scalaz._

/** A control file, not to be loaded into HDFS */
case class Control(file: File)

/** A file to be loaded into HDFS */
case class Data(file: File, fileSubDir: File)

/** Files found by [[findFilese]] */
case class InputFiles(controlFiles: List[Control], dataFiles: List[Data])

/** Factory for `Input`s */
object Input {

  /**
    * Find files from a directory matching a given file pattern.
    *
    * See the description of the file pattern at [[au.cba.com.omnia.meastro.core.task.Upload]]
    */
  def findFiles(
    sourceDir: String, tablename: String, filePattern: String, controlPattern: Regex
  ): String \/ InputFiles = for {
    matcher <- PatternParsers.makeFileNameMatcher(tablename, filePattern)
    files   <- fromNullable((new File(sourceDir)).listFiles, s"${sourceDir} does not exist").map(_.toList)
    results <- files traverse (file => matcher(file.getName))
  } yield {
    val matches      = (files zip results) collect { case (file, Match(dirs)) => (file, dirs) }
    val (ctrl, data) = matches partition { case (file, _) => isControl(file, controlPattern) }
    val controls     = ctrl map { case (file, _)    => Control(file) }
    val dataFiles    = data map { case (file, dirs) => Data(file, new File(dirs mkString File.separator)) }
    InputFiles(controls, dataFiles)
  }

  def isControl(file: File, controlPattern: Regex) =
    controlPattern.unapplySeq(file.getName).isDefined

  def fromNullable[A](v: A, msg: String) =
    if (v == null) msg.left else v.right
}
