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

import scala.util.control.NonFatal

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader

import org.joda.time.{DateTime, DateTimeFieldType, MutableDateTime}
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}

import scalaz._, Scalaz._

import au.com.cba.omnia.omnitool.time.DateFormatInfo

/**
  * Base class containing Parser type and some utility methods
  * for parsing.
  */
trait ParserBase extends Parsers {

  type Elem = Char

  /** Either a value or an error message */
  type MayFail[A] = String \/ A

  /** Factory functions for MayFail */
  object MayFail {

    /** convert an option to a disjunction */
    def fromOption[A](option: Option[A], msg: String): MayFail[A] =
      option match {
        case Some(a) => a.right
        case None    => msg.left
      }

    /** Fail if condition is false */
    def guard(cond: Boolean, msg: String): MayFail[Unit] =
      if (cond) ().right else msg.left

    /** Safely run an operation that may throw an exception */
    def safe[A](a: => A): MayFail[A] =
      try { a.right } catch { case NonFatal(ex) => ex.toString.left }
  }

  /** Throws parse error (not parse failure) if disjunction failed */
  def mandatory[A](x : MayFail[A]): Parser[A] =
    x.fold(msg => err(msg), a => success(a))

  /** Succeeds if we are at end of file */
  val eof =
    Parser(in => if (in.atEnd) Success((), in) else Failure("not at EOF", in))

  /** The character used to escape other special characters */
  val esc       = '\\'

  /** Parse a single character, which may be escaped */
  def escape(specialChars: Char*) = {
    val allSpecial = esc :: specialChars.toList
    val normal     = elem("normal char", !allSpecial.contains(_))
    val special    = elem("special char", allSpecial.contains(_))
    val escaped    = accept(esc) ~> special
    normal | escaped
  }


}
