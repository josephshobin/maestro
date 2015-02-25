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

import scala.util.parsing.input.CharSequenceReader

import org.joda.time.{DateTimeFieldType, MutableDateTime}
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
  * corresponding to the fields in the pattern.
  */
case class Match(dirs: List[String]) extends MatchResult

/** Contains makeFileNameParser, the FileNameMatcher, and the PartialParser */
trait FileNameMatchers extends ParserBase {

  /** Apply a pattern parser to a pattern and get a file name parsing function back. */
  def makeFileNameMatcher(patternParser: Parser[FileNameMatcher], pattern: String): MayFail[FileNameMatcher] =
    patternParser(new CharSequenceReader(pattern)) match {
      case NoSuccess(msg, _)  => msg.left
      case Success(parser, _) => parser.right
    }

  /**
    * Input file name parser
    *
    * Parses a file name and returns the directories which the file should be
    * placed in.
    */
  case class FileNameMatcher(parser: Parser[List[String]]) {
    def apply(fileName: String): MayFail[MatchResult] =
      parser(new CharSequenceReader(fileName)) match {
        case Error(msg, _)    => msg.left
        case Failure(_, _)    => NoMatch.right
        case Success(dirs, _) => Match(dirs).right
      }
  }

  /** Factory function for FileNameMatchers */
  object FileNameMatcher {

    /** creates a FileNameMatcher from a list of PartialParser's */
    def fromParts(parts: List[PartialParser]): MayFail[FileNameMatcher] =
      validateTimeFields(parts).map(timeFields => {
        val finished    = eof ^^ (_ => Fields(Map.empty[DateTimeFieldType, Int], List.empty[String]))
        val fieldParser = parts.foldRight(finished)((part, parser) => part.extend(parser))
        val fileParser  = fieldParser ^^ (fields => {
          val timeDirs = timeFields.map(field => timeDir(field, fields.time(field)))
          timeDirs ++ fields.misc
        })
        FileNameMatcher(fileParser)
      })

    def timeDir(field: DateTimeFieldType, value: Int) =
      if (field equals DateTimeFieldType.year) f"$value%04d" else f"$value%02d"

    def validateTimeFields(parts: List[PartialParser]): MayFail[List[DateTimeFieldType]] = {
      val fields = parts.flatMap(_.timeFields).distinct
      for {
        _           <- MayFail.guard(fields.nonEmpty, s"no date time fields found")
        fieldOrders <- fields.traverse[MayFail, Int](fieldOrder(_))
        expected     = uploadFields.take(fieldOrders.max + 1)
        missing      = expected.filter(!fields.contains(_))
        _           <- MayFail.guard(missing.empty, s"missing date time fields: ${missing.mkString(", ")}")
      } yield expected
    }

    val uploadFields = List(
      DateTimeFieldType.year,
      DateTimeFieldType.monthOfYear,
      DateTimeFieldType.dayOfMonth,
      DateTimeFieldType.hourOfDay,
      DateTimeFieldType.minuteOfHour,
      DateTimeFieldType.secondOfMinute
    )

    def fieldOrder(t: DateTimeFieldType) = {
      val index = uploadFields indexOf t
      if (index >= 0) index.right
      else            s"Upload does not accept $t fields".left
    }
  }

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

  /** We allow unknown characters (?) and wildcards (*) in misc fields */
  sealed trait MiscFieldItem
  case object Unknown extends MiscFieldItem
  case object Wildcard extends MiscFieldItem

  /**
    * Parses part of a file name.
    *
    * The extend field takes the parser thay parses the rest of the file,
    * and extends it to parse this part of the file too.
    */
  case class PartialParser(timeFields: List[DateTimeFieldType], extend: Parser[Fields] => Parser[Fields])

  /** Factory for partial file name parsers */
  object PartialParser {

    val noFields = List.empty[DateTimeFieldType]

    /** Create a PartialParser from a parser which does not produce any Fields info */
    def empty(parser: Parser[_]) =
      PartialParser(noFields, continuation => parser ~> continuation)

    /** Partial file name parser expecting a literal */
    def literal(lit: String) = empty(acceptSeq(lit))

    /** PartialParser for matching any single char */
    val unknown = empty(next)

    /** PartialParser consuming the largest sequence of characters for the rest of the glob to succeed */
    val unknownSeq = PartialParser(noFields, continuation => {
      def parser: Parser[Fields] = (next ~> parser) | continuation
      parser
    })

    /**
      * PartialParser which stores a list of wildcards as a misc field
      *
      * With this way of implementing wildcards by passing around the continuation
      * parser, I am not sure how to avoid duplicating logic with unknownSeq above.
      */
    def miscField(items: List[MiscFieldItem]) = PartialParser(noFields, continuation => {
      def makeParser(items: List[MiscFieldItem]): Parser[(String, Fields)] = items match {
        case Nil => continuation ^^ (fields => ("", fields))

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

      makeParser(items) ^? {
        case (miscField, fields) if !miscField.isEmpty => Fields(fields.time, miscField :: fields.misc)
      }
    })

    /** Input parser expecting a timestamp following a joda-time pattern */
    def timestamp(pattern: String): MayFail[PartialParser] = for {
      formatter <- MayFail.safe(DateTimeFormat.forPattern(pattern))
      fields    <- MayFail.fromOption(DateFormatInfo.fields(formatter), s"Could not find fields in date time pattern <$pattern>.")
    } yield {
      val parser = timestampParser(formatter, fields)
      val extend = (continuation: Parser[Fields]) => for {
        fields1 <- parser
        fields2 <- continuation
        fields3 <- mandatory(fields1 followedBy fields2)
      } yield fields3
      PartialParser(fields, extend)
    }

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
  }
}
