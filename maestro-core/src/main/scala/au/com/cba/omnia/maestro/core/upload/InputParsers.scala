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

/** The results of a match against a file name */
sealed trait MatchResult

/** The file does not match the pattern */
case object NoMatch extends MatchResult

/**
  * The file matches the pattern.
  *
  * Also contains the directories for the file
  * corresponding to the date time fields in the pattern.
  */
case class Match(dirs: List[String]) extends MatchResult

/**
  * Contains parsers for input file patterns and input files
  *
  * The `forPattern` method is the most important one for users.
  * This method parses a file pattern and produces an appropriate
  * file name matcher.
  */
object InputParsers extends Parsers {

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

  /**
    * Pass in table name and file pattern and get back a matching function on file names.
    *
    * Both the pattern parsing and the matching functions can fail with an error message.
    */
  def forPattern(tableName: String, pattern: String): MayFail[String => MayFail[MatchResult]] =
    PatternParser.pattern(tableName)(new CharSequenceReader(pattern)) match {
      case NoSuccess(msg, _) => {
        msg.left
      }
      case Success(fileNameParser, _) => {
        val func: String => MayFail[MatchResult] =
          (fileName: String) => fileNameParser(new CharSequenceReader(fileName)) match {
            case Error(msg, _)    => msg.left
            case Failure(_, _)    => NoMatch.right
            case Success(dirs, _) => Match(dirs).right
          }
        func.right
      }
    }

  /** Throws parse error (not parse failure) if disjunction failed */
  def mandatory[A](x : MayFail[A]): Parser[A] =
    x.fold(msg => err(msg), a => success(a))

  type Elem = Char

  /**
    * File pattern parser
    *
    * Parses part of a file pattern and produces a function
    * that extends the partial parser to the right.
    */
  type PatternParser = Parser[PartialParser => PartialParser]

  /**
    * Input file name parser
    *
    * Parses a file name and returns the directories which the file should be
    * placed in.
    */
  type FileNameParser = Parser[List[String]]

  /**
    * Fields in some part of the file name.
    *
    * misc is a list of miscellaneous fields in the same order
    * that they appear in the file name.
    * */
  case class Fields(time: Map[DateTimeFieldType, Int], misc: List[String]) {
    def followedBy(that: Fields): MayFail[Fields] = {
      val timeVals = this.time.mapValues(List(_)) |+| that.time.mapValues(List(_))
      val fields   = timeVals.toList.traverse[MayFail, (DateTimeFieldType, Int)] { case (k,vs) => vs.distinct match {
        case List(v)   => (k,v).right
        case conflicts => s"conflicting values found for $k: ${vs.mkString(", ")}".left
      }}
      fields.map(fs => Fields(fs.toMap, this.misc ++ that.misc))
    }
  }

  /**
    * We allow unknown characters (?) and wildcards (*) in misc fields
    */
  sealed trait MiscFieldItem
  case object Unknown extends MiscFieldItem
  case object Wildcard extends MiscFieldItem

  /**
    * Building block of an input file name parser.
    *
    * Has a pre-determined list of date time fields it sets. Some primitive
    * input parsers have nothing in their `fields` list. This is fine: it
    * indicates the date times their parsers return are dummy values with no
    * fields set.
    */
  case class PartialParser(fields: List[DateTimeFieldType], parser: Parser[Fields])
      extends Parser[Fields] {
    def apply(in: Input) = parser(in)

    /**
      * Sequential composition with another partial file name parser
      *
      * It is a parse error (not failure) if the date time fields are consistent.
      */
    def followedBy(that: PartialParser) = {
      val combinedFields = (this.fields ++ that.fields).distinct
      val combinedParser = for {
        fields1 <- this.parser
        fields2 <- that.parser
        fields3 <- mandatory(fields1 followedBy fields2)
      } yield fields3
      PartialParser(combinedFields, combinedParser)
    }
  }

  /** Succeeds if we are at end of file */
  val eof =
    Parser(in => if (in.atEnd) Success((), in) else Failure("not at EOF", in))

  /**
    * List of `PatternParser`s
    *
    * The important one is `pattern`: this parses the whole file pattern
    * and returns the complete file name parser
    */
  object PatternParser {
    val esc       = '\\'
    val one       = '?'
    val any       = '*'
    val start     = '{'
    val end       = '}'
    val tableSign = "table"

    /** Surround a parser with curly brackets */
    def surround[U](parser: Parser[U]) =
      accept(start) ~> parser <~ accept(end)

    /** Parse a single character, which may be escaped */
    def escape(specialChars: Char*) = {
      val allSpecial = esc :: specialChars.toList
      val normal     = elem("normal char", !allSpecial.contains(_))
      val special    = elem("special char", allSpecial.contains(_))
      val escaped    = accept(esc) ~> special
      normal | escaped
    }

    /** Parses a literal string of characters */
    val literal: PatternParser =
      rep1(escape(one, any, start, end)) ^^ (lits => PartialParser.literal(lits.mkString))

    /** Parses the {table} construct */
    def table(tableName: String): PatternParser =
      surround(acceptSeq(tableSign)) ^^ (_ => PartialParser.literal(tableName))

    /** Parses a miscellaneous field */
    val miscField: PatternParser = {
      val unknownItem  = accept(one)       ^^ (_ => Unknown)
      val wildcardItem = rep1(accept(any)) ^^ (_ => Wildcard)
      surround(rep1(unknownItem | wildcardItem)) ^^ (items => PartialParser.miscField(items))
    }

    /** Parse a timestamp. We should try table and miscField first. */
    val timestamp: PatternParser = for {
      tsPattern <- surround(rep(escape(end))) ^^ (_.mkString)
      fnParser  <- if (tsPattern == tableSign) failure("'table' matches the table name, not a date time")
                   else                        mandatory(PartialParser.timestamp(tsPattern))
    } yield fnParser

    /** Parse a question mark */
    val unknown: PatternParser =
      accept(one) ^^ (_ => PartialParser.unknown)

    /** Parser which succeeds if we have consumed the entire file pattern */
    val finished: Parser[PartialParser] =
      eof ^^ (_ => PartialParser.finished)

    /** Parser which consumes a wildcard */
    val unknownSeq: PatternParser =
      rep1(accept(any)) ^^ (_ => PartialParser.unknownSeq)

    /** Match a single element element */
    def part(tableName: String): PatternParser =
      literal | table(tableName) | miscField | timestamp | unknown | unknownSeq

    /** Parses a file pattern, returning the subsequent file name parser */
    def pattern(tableName: String): Parser[FileNameParser] = for {
      raw       <- rep(part(tableName)) <~ eof ^^ (_.foldRight(PartialParser.finished)((modifier, continuation) => modifier(continuation)))
      validated <- mandatory(PartialParser.validate(raw))
    } yield validated
  }

  /** Factory for partial file name parsers */
  object PartialParser {
    /** Create a PartialParser => PartialParser from a parser which does not produce any Fields info */
    def empty(parser: Parser[_]) =
      (continuation: PartialParser) => PartialParser(continuation.fields, parser ~> continuation.parser)

    /** Partial file name parser expecting a literal */
    def literal(lit: String) = empty(acceptSeq(lit))

    /** PartialParser for matching any single char */
    val unknown = empty(next)

    /** PartialParser consuming the largest sequence of characters for the rest of the glob to succeed */
    def unknownSeq(continuation: PartialParser) = {
      // rhs of ~> is lazy, so parser does not recurse indefinitely
      def parser: Parser[Fields] = (next ~> parser) | continuation.parser
      PartialParser(continuation.fields, parser)
    }

    /**
      * PartialParser which stores a list of wildcards as a misc field
      *
      * With this way of implementing wildcards by passing around the continuation
      * parser, I am not sure how to avoid duplicating logic with unknownSeq above.
      */
    def miscField(items: List[MiscFieldItem]) =
      (continuation: PartialParser) => {
        def makeParser(items: List[MiscFieldItem]): Parser[(String, Fields)] = items match {
          case Nil => continuation.parser ^^ (fields => ("", fields))

          case (Unknown :: restItems) => for {
            chr                 <- next
            (restField, fields) <- makeParser(restItems)
          } yield (chr.toString + restField, fields)

          case currItems @ (Wildcard :: restItems) => {
            val wildcardMove = for {
              chr                 <- next
              (restField, fields) <- makeParser(currItems)
            } yield (chr.toString + restField, fields)
            val wildcardStop = makeParser(restItems)
            wildcardMove | wildcardStop
          }
        }

        val parser = makeParser(items) ^? {
          case (miscField, fields) if !miscField.isEmpty => Fields(fields.time, miscField :: fields.misc)
        }
        PartialParser(continuation.fields, parser)
      }

    /** Input parser expecting a timestamp following a joda-time pattern */
    def timestamp(pattern: String): MayFail[PartialParser => PartialParser] = for {
      formatter <- MayFail.safe(DateTimeFormat.forPattern(pattern))
      fields    <- MayFail.fromOption(DateFormatInfo.fields(formatter), s"Could not find fields in date time pattern <$pattern>.")
    } yield PartialParser(fields, timestampParser(formatter, fields)).followedBy

    def timestampParser(formatter: DateTimeFormatter, fields: List[DateTimeFieldType]): Parser[Fields] =
      Parser(in => in match {
        // if we have underlying string, we can convert DateTimeFormatter.parseInto method into a scala Parser
        case _: CharSequenceReader => {
          if (in.atEnd)
            Failure("Cannot parse date time when at end of input", in)
          else {
            val underlying = in.source.toString
            val pos        = in.offset
            val dateTime   = new MutableDateTime(0)
            val parseRes   = MayFail.safe(formatter.parseInto(dateTime, underlying, pos))
            parseRes match {
              case \/-(newPos) if newPos >= 0 => {
                val timeFields = fields.map(field => (field, dateTime.get(field)))
                val negFields  = timeFields filter { case (field, value) => value < 0 } map { case (field, value) => field }
                if (negFields.nonEmpty) Failure(s"Negative fields: ${negFields.mkString(", ")}", in)
                else                    Success(Fields(timeFields.toMap, List.empty[String]), new CharSequenceReader(underlying, newPos))
              }
              case \/-(failPosCompl) => {
                val failPos     = ~failPosCompl
                val beforeStart = underlying.substring(0, pos)
                val beforeFail  = underlying.substring(pos, failPos)
                val afterFail   = underlying.substring(failPos, underlying.length)
                val msg         = s"Failed to parse date time. Date time started at the '@' and failed at the '!' here: $beforeStart @ $beforeFail ! $afterFail"
                Failure(msg, new CharSequenceReader(underlying, failPos))
              }
              case -\/(msg) => {
                Error(msg, in)
              }
            }
          }
        }

        // if we don't have underlying string, we're hosed
        case _ => Error(s"PartialParser only works on CharSequenceReaders", in)
      })

    /** Partial file name parser that succeeds when at the end of the file name */
    val finished = PartialParser(List.empty[DateTimeFieldType], eof ^^ (_ => Fields(Map.empty[DateTimeFieldType, Int], List.empty[String])))

    /** Take the next character */
    def next = elem("char", _ => true)

    /**
      * Turns a PartialParser into a FileNameParser.
      *
      * Checks that the fields in the file pattern are valid. The restrictions
      * on file pattern fields are described in
      * [[au.cba.com.omnia.maestro.core.task.Upload]].
      */
    def validate(parser: PartialParser): MayFail[FileNameParser] = for {
      _           <- MayFail.guard(parser.fields.nonEmpty, s"no date time fields found")
      fieldOrders <- parser.fields.traverse[MayFail, Int](fieldOrder(_))
      expected     = uploadFields.take(fieldOrders.max + 1)
      missing      = expected.filter(!parser.fields.contains(_))
      _           <- MayFail.guard(missing.empty, s"missing date time fields: ${missing.mkString(", ")}")
    } yield parser ^^ (fields => {
      val timeDirs = expected.map(field => {
        val value = fields.time(field)
        if (field equals DateTimeFieldType.year) f"$value%04d" else f"$value%02d"
      })
      timeDirs ++ fields.misc
    })

    /** Ordered list of fields we permit in upload time stamps */
    val uploadFields = List(
      DateTimeFieldType.year,
      DateTimeFieldType.monthOfYear,
      DateTimeFieldType.dayOfMonth,
      DateTimeFieldType.hourOfDay,
      DateTimeFieldType.minuteOfHour,
      DateTimeFieldType.secondOfMinute
    )

    /** Assign numbers to each field that upload works with */
    def fieldOrder(t: DateTimeFieldType) = {
      val index = uploadFields indexOf t
      if (index >= 0) index.right
      else            s"Upload does not accept $t fields".left
    }
  }
}
