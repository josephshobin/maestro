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
  can parse {table}-{yyyy-MM-dd}-{??}*    $parseMiscField
  can parse {yyyyMMdd}{*??*?**}           $parseCrazyMiscField
  can parse {*}-{ddMMyy}-{*}              $parseTwoMiscFields

failing file pattern parsing
----------------------------

  fail on {table}                         $expectOneTimestamp
  fail on {table}{yyyydd}                 $expectContiguousFields
  fail on {table}{MMdd}                   $expectYear
  fail on conflicting field values        $expectConstantFieldValue
  fail on invalid field values            $expectValidFieldValues
"""

  def parseUpToDay =
    PatternParsers.makeFileNameMatcher("mytable", "{table}{yyyyMMdd}") must beLike {
      case \/-(matcher) => {
        matcher("mytable20140807") must_== \/-(Match(List("2014", "08", "07")))
        matcher("mytable20140830") must_== \/-(Match(List("2014", "08", "30")))
      }
    }

  def parseUpToSecond =
    PatternParsers.makeFileNameMatcher("mytable", "{table}{yyyyMMddHHmmss}") must beLike {
      case \/-(matcher) => {
        matcher("mytable20140807203000") must_== \/-(Match(List("2014", "08", "07", "20", "30", "00")))
      }
    }

  def parseDifferentFieldOrder =
    PatternParsers.makeFileNameMatcher("foobar", "{table}{yyyyddMM}") must beLike {
      case \/-(matcher) => {
        matcher("foobar20140708") must_== \/-(Match(List("2014", "08", "07")))
      }
    }

  def parseShortYear =
    PatternParsers.makeFileNameMatcher("foobar", "{table}{yyMMdd}") must beLike {
      case \/-(matcher) => {
        matcher("foobar140807") must_== \/-(Match(List("2014", "08", "07")))
      }
    }

  def parseLiterals =
    PatternParsers.makeFileNameMatcher("credit", "{table}_{yyyy-MM-dd-HH}") must beLike {
      case \/-(matcher) => {
        matcher("credit_2014-08-07-20") must_== \/-(Match(List("2014", "08", "07", "20")))
      }
    }

  def parseDifferentElementOrder =
    PatternParsers.makeFileNameMatcher("credit", "{ddMMyyyy}{table}") must beLike {
      case \/-(matcher) => {
        matcher("07082014credit") must_== \/-(Match(List("2014", "08", "07")))
      }
    }

  def parseWildcards =
    PatternParsers.makeFileNameMatcher("mytable", "{table}*{yyyyMMdd}*") must beLike {
      case \/-(matcher) => {
        matcher("mytable-foobar-2014-201408079999.foobar") must_== \/-(Match(List("2014", "08", "07")))
      }
    }

  def wildcardMatchLarge =
    PatternParsers.makeFileNameMatcher("mytable", "*{yyyyMMdd}*") must beLike {
      case \/-(matcher) => {
        matcher("foo-20141225-bar-20150224.txt") must_== \/-(Match(List("2015", "02", "24")))
      }
    }

  def parseQuestionMarks =
    PatternParsers.makeFileNameMatcher("mytable", "{table}??{yyyyMMdd}") must beLike {
      case \/-(matcher) => {
        matcher("mytable--20140807") must_== \/-(Match(List("2014", "08", "07")))
        matcher("mytable0020140807") must_== \/-(Match(List("2014", "08", "07")))
      }
    }

  def parseDuplicateTimestamps =
    PatternParsers.makeFileNameMatcher("cars", "{table}_{yyyyMMdd}_{MMyyyy}") must beLike {
      case \/-(matcher) => {
        matcher("cars_20140807_082014") must_== \/-(Match(List("2014", "08", "07")))
      }
    }

  def parseCombinedTimestampFields =
    PatternParsers.makeFileNameMatcher("cars", "{yyyy}_{table}_{MMdd}") must beLike {
      case \/-(matcher) => {
        matcher("2014_cars_0807") must_== \/-(Match(List("2014", "08", "07")))
      }
    }

  def parseMiscField =
    PatternParsers.makeFileNameMatcher("mytable", "{table}-{yyyy-MM-dd}-{??}*") must beLike {
      case \/-(matcher) => {
        matcher("mytable-2015-02-24-a1.DAT") must_== \/-(Match(List("2015", "02", "24", "a1")))
      }
    }

  def parseCrazyMiscField =
    PatternParsers.makeFileNameMatcher("mytable", "{yyyyMMdd}{*??*?**}") must beLike {
      case \/-(matcher) => {
        matcher("20150224foo") must_== \/-(Match(List("2015", "02", "24", "foo")))
      }
    }

  def parseTwoMiscFields =
    PatternParsers.makeFileNameMatcher("foo", "{*}-{ddMMyy}-{*}") must beLike {
      case \/-(matcher) => {
        matcher("000-240215-bbb") must_== \/-(Match(List("2015", "02", "24", "000", "bbb")))
      }
    }

  def expectOneTimestamp =
    PatternParsers.makeFileNameMatcher("marketing", "{table}") must beLike {
      case -\/(_) => ok
    }

  def expectContiguousFields =
    PatternParsers.makeFileNameMatcher("marketing", "{table}{yyyydd}") must beLike {
      case -\/(_) => ok
    }

  def expectYear =
    PatternParsers.makeFileNameMatcher("marketing", "{table}{MMdd}") must beLike {
      case -\/(_) => ok
    }

  def expectConstantFieldValue =
    PatternParsers.makeFileNameMatcher("dummy", "{table}_{yyMM}_{yyMM}") must beLike {
      case \/-(matcher) => {
        matcher("dummy_1408_1401") must beLike { case -\/(_) => ok }
      }
    }

  def expectValidFieldValues =
    PatternParsers.makeFileNameMatcher("dummy", "{table}{yyyyMMdd}") must beLike {
      case \/-(matcher) => {
        matcher("dummy20140231") must beLike { case -\/(_) => ok }
      }
    }
}
