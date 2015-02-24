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

import org.specs2.Specification

import java.io.File

import org.joda.time.DateTime

import scalaz.{\/-, -\/}

class InputParsersSpec extends Specification { def is = s2"""
successfull file pattern parsing
--------------------------------

  can parse {table}{yyyyMMdd}             $parseUpToDay
  can parse {table}{yyyyMMddHHmmss}       $parseUpToSecond
  can parse {table}{yyyyddMM}             $parseDifferentFieldOrder
  can parse {table}{yyMMdd}               $parseShortYear
  can parse {table}_{yyyy-MM-dd-HH}       $parseLiterals
  can parse {ddMMyyyy}{table}             $parseDifferentElementOrder
  can parse {table}*{yyyyMMdd}*           $parseWildcards
  wildcard match is the largest possible  $wildcardMatchLarge
  can parse {table}??{yyyyMMdd}           $parseQuestionMarks
  can parse {table}_{yyyyMMdd}_{MMyyyy}   $parseDuplicateTimestamps
  can parse {yyyy}_{table}_{MMdd}         $parseCombinedTimestampFields

failing file pattern parsing
----------------------------

  fail on {table}                         $expectOneTimestamp
  fail on {table}{yyyydd}                 $expectContiguousFields
  fail on {table}{MMdd}                   $expectYear
  fail on conflicting field values        $expectConstantFieldValue
  fail on invalid field values            $expectValidFieldValues
"""

  def parseUpToDay =
    InputParsers.forPattern("mytable", "{table}{yyyyMMdd}") must beLike {
      case \/-(matcher) => {
        matcher("mytable20140807") must_== \/-(Match(List("2014", "08", "07")))
        matcher("mytable20140830") must_== \/-(Match(List("2014", "08", "30")))
      }
    }

  def parseUpToSecond =
    InputParsers.forPattern("mytable", "{table}{yyyyMMddHHmmss}") must beLike {
      case \/-(matcher) => {
        matcher("mytable20140807203000") must_== \/-(Match(List("2014", "08", "07", "20", "30", "00")))
      }
    }

  def parseDifferentFieldOrder =
    InputParsers.forPattern("foobar", "{table}{yyyyddMM}") must beLike {
      case \/-(matcher) => {
        matcher("foobar20140708") must_== \/-(Match(List("2014", "08", "07")))
      }
    }

  def parseShortYear =
    InputParsers.forPattern("foobar", "{table}{yyMMdd}") must beLike {
      case \/-(matcher) => {
        matcher("foobar140807") must_== \/-(Match(List("2014", "08", "07")))
      }
    }

  def parseLiterals =
    InputParsers.forPattern("credit", "{table}_{yyyy-MM-dd-HH}") must beLike {
      case \/-(matcher) => {
        matcher("credit_2014-08-07-20") must_== \/-(Match(List("2014", "08", "07", "20")))
      }
    }

  def parseDifferentElementOrder =
    InputParsers.forPattern("credit", "{ddMMyyyy}{table}") must beLike {
      case \/-(matcher) => {
        matcher("07082014credit") must_== \/-(Match(List("2014", "08", "07")))
      }
    }

  def parseWildcards =
    InputParsers.forPattern("mytable", "{table}*{yyyyMMdd}*") must beLike {
      case \/-(matcher) => {
        matcher("mytable-foobar-2014-201408079999.foobar") must_== \/-(Match(List("2014", "08", "07")))
      }
    }

  def wildcardMatchLarge =
    InputParsers.forPattern("mytable", "*{yyyyMMdd}*") must beLike {
      case \/-(matcher) => {
        matcher("foo-20141225-bar-20150224.txt") must_== \/-(Match(List("2015", "02", "24")))
      }
    }

  def parseQuestionMarks =
    InputParsers.forPattern("mytable", "{table}??{yyyyMMdd}") must beLike {
      case \/-(matcher) => {
        matcher("mytable--20140807") must_== \/-(Match(List("2014", "08", "07")))
        matcher("mytable0020140807") must_== \/-(Match(List("2014", "08", "07")))
      }
    }

  def parseDuplicateTimestamps =
    InputParsers.forPattern("cars", "{table}_{yyyyMMdd}_{MMyyyy}") must beLike {
      case \/-(matcher) => {
        matcher("cars_20140807_082014") must_== \/-(Match(List("2014", "08", "07")))
      }
    }

  def parseCombinedTimestampFields =
    InputParsers.forPattern("cars", "{yyyy}_{table}_{MMdd}") must beLike {
      case \/-(matcher) => {
        matcher("2014_cars_0807") must_== \/-(Match(List("2014", "08", "07")))
      }
    }

  def expectOneTimestamp =
    InputParsers.forPattern("marketing", "{table}") must beLike {
      case -\/(_) => ok
    }

  def expectContiguousFields =
    InputParsers.forPattern("marketing", "{table}{yyyydd}") must beLike {
      case -\/(_) => ok
    }

  def expectYear =
    InputParsers.forPattern("marketing", "{table}{MMdd}") must beLike {
      case -\/(_) => ok
    }

  def expectConstantFieldValue =
    InputParsers.forPattern("dummy", "{table}_{yyMM}_{yyMM}") must beLike {
      case \/-(matcher) => {
        matcher("dummy_1408_1401") must beLike { case -\/(_) => ok }
      }
    }

  def expectValidFieldValues =
    InputParsers.forPattern("dummy", "{table}{yyyyMMdd}") must beLike {
      case \/-(matcher) => {
        matcher("dummy20140231") must beLike { case -\/(_) => ok }
      }
    }
}
