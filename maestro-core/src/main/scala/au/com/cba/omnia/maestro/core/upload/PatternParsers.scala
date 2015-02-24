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
  *  Contains `makePatternParser` and `PartialPatternParser`
  */
trait PatternParsers extends FileNameParsers {

  /** Create a parser which parses a pattern and returns the file name parser */
  def makePatternParser(tableName: String): Parser[FileNameParser] = for {
    raw       <- rep(PartialPatternParser.part(tableName)) <~ eof ^^ (_.foldRight(PartialParser.finished)((modifier, continuation) => modifier(continuation)))
    validated <- mandatory(PartialParser.validate(raw))
  } yield validated

  /**
    * Partial pattern parser
    *
    * Parses part of a pattern and produces the corresponding parser for the
    * corresponding part of the file name. However, we don't directly produce
    * that parser. Instead, we return a function that takes the parser to be run
    * on the rest of the file name, and returns the parser that runs on this
    * part of the file name plus the rest.
    *
    * This indirection is used for wildcards, where the amount of file name we
    * need to match depends on the rest of the parser successfully parsing the
    * rest of the file name.
    */
  type PartialPatternParser = Parser[PartialParser => PartialParser]

  /** Factory for `PartialPatternParser`s */
  object PartialPatternParser {
    val one       = '?'
    val any       = '*'
    val start     = '{'
    val end       = '}'
    val tableSign = "table"

    /** Surround a parser with brackets of some sort */
    def surround[U](parser: Parser[U]) =
      accept(start) ~> parser <~ accept(end)

    /** Parses a literal string of characters */
    val literal: PartialPatternParser =
      rep1(escape(one, any, start, end)) ^^ (lits => PartialParser.literal(lits.mkString))

    /** Parses the {table} construct */
    def table(tableName: String): PartialPatternParser =
      surround(acceptSeq(tableSign)) ^^ (_ => PartialParser.literal(tableName))

    /** Parses a miscellaneous field */
    val miscField: PartialPatternParser = {
      val unknownItem  = accept(one)       ^^ (_ => Unknown)
      val wildcardItem = rep1(accept(any)) ^^ (_ => Wildcard)
      surround(rep1(unknownItem | wildcardItem)) ^^ (items => PartialParser.miscField(items))
    }

    /** Parse a timestamp. We should try table and miscField first. */
    val timestamp: PartialPatternParser = for {
      tsPattern <- surround(rep(escape(end))) ^^ (_.mkString)
      fnParser  <- if (tsPattern == tableSign) failure("'table' matches the table name, not a date time")
                   else                        mandatory(PartialParser.timestamp(tsPattern))
    } yield fnParser

    /** Parse a question mark */
    val unknown: PartialPatternParser =
      accept(one) ^^ (_ => PartialParser.unknown)

    /** Parser which consumes a wildcard */
    val unknownSeq: PartialPatternParser =
      rep1(accept(any)) ^^ (_ => PartialParser.unknownSeq)

    /** Match a single element element */
    def part(tableName: String): PartialPatternParser =
      literal | table(tableName) | miscField | timestamp | unknown | unknownSeq
  }
}
