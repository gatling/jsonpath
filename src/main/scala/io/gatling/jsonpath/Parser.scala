/**
 * Copyright 2011-2017 GatlingCorp (http://gatling.io)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.gatling.jsonpath

import java.lang.{ StringBuilder => JStringBuilder }

import scala.util.parsing.combinator.RegexParsers

import io.gatling.jsonpath.AST._

object FastStringOps {
  implicit class RichString(val text: String) extends AnyVal {
    def fastReplaceAll(searchString: String, replacement: String): String = {
      if (searchString.isEmpty || replacement.isEmpty) {
        text
      } else {
        var start = 0
        var end = text.indexOf(searchString, start)
        if (end == -1) {
          text
        } else {
          val buf = new JStringBuilder(text.length)
          while (end != -1) {
            buf.append(text, start, end).append(replacement)
            start = end + searchString.length
            end = text.indexOf(searchString, start)
          }
          buf.append(text, start, text.length)
          buf.toString
        }
      }
    }
  }
}

trait ParserBase extends RegexParsers {

  val NumberRegex = """-?\d+""".r
  val FieldRegex = """[$_\d\p{L}][$_\-\d\p{L}]*""".r
  val SingleQuotedFieldRegex = """(\\.|[^'])+""".r
  val DoubleQuotedFieldRegex = """(\\.|[^"])+""".r
  val SingleQuotedValueRegex = """(\\.|[^'])*""".r
  val DoubleQuotedValueRegex = """(\\.|[^"])*""".r
  val NumberValueRegex = """-?\d+(\.\d*)?""".r

  /// general purpose parsers ///////////////////////////////////////////////

  def number: Parser[Int] = NumberRegex ^^ (_.toInt)

  def field: Parser[String] = FieldRegex

  import FastStringOps._
  def singleQuotedField = "'" ~> SingleQuotedFieldRegex <~ "'" ^^ (_.fastReplaceAll("\\'", "'"))
  def doubleQuotedField = "\"" ~> DoubleQuotedFieldRegex <~ "\"" ^^ (_.fastReplaceAll("\\\"", "\""))
  def singleQuotedValue = "'" ~> SingleQuotedValueRegex <~ "'" ^^ (_.fastReplaceAll("\\'", "'"))
  def doubleQuotedValue = "\"" ~> DoubleQuotedValueRegex <~ "\"" ^^ (_.fastReplaceAll("\\\"", "\""))
  def quotedField: Parser[String] = singleQuotedField | doubleQuotedField
  def quotedValue: Parser[String] = singleQuotedValue | doubleQuotedValue

  /// array parsers /////////////////////////////////////////////////////////

  def arraySliceStep: Parser[Option[Int]] = ":" ~> number.?

  def arraySlice: Parser[ArraySlice] =
    (":" ~> number.?) ~ arraySliceStep.? ^^ {
      case end ~ step => ArraySlice(None, end, step.flatten.getOrElse(1))
    }

  def arrayRandomAccess: Parser[Option[ArrayRandomAccess]] =
    rep1("," ~> number).? ^^ (indices => indices.map(ArrayRandomAccess))

  def arraySlicePartial: Parser[ArrayAccessor] =
    number ~ arraySlice ^^ {
      case i ~ as => as.copy(start = Some(i))
    }

  def arrayRandomAccessPartial: Parser[ArrayAccessor] =
    number ~ arrayRandomAccess ^^ {
      case i ~ None                             => ArrayRandomAccess(i :: Nil)
      case i ~ Some(ArrayRandomAccess(indices)) => ArrayRandomAccess(i :: indices)
    }

  def arrayPartial: Parser[ArrayAccessor] =
    arraySlicePartial | arrayRandomAccessPartial

  def arrayAll: Parser[ArraySlice] =
    "*" ^^ (_ => ArraySlice(None, None))

  def arrayAccessors: Parser[ArrayAccessor] =
    "[" ~> (arrayAll | arrayPartial | arraySlice) <~ "]"

  /// filters parsers ///////////////////////////////////////////////////////

  def numberValue: Parser[JPNumber] = NumberValueRegex ^^ {
    s => if (s.indexOf('.') != -1) JPDouble(s.toDouble) else JPLong(s.toLong)
  }

  def booleanValue: Parser[FilterDirectValue] =
    "true" ^^ (_ => JPTrue) |
      "false" ^^ (_ => JPFalse)

  def nullValue: Parser[FilterValue] =
    "null" ^^ (_ => JPNull)

  def stringValue: Parser[JPString] = quotedValue ^^ { JPString }
  def value: Parser[FilterValue] = booleanValue | numberValue | nullValue | stringValue

  def comparisonOperator: Parser[ComparisonOperator] =
    "==" ^^ (_ => EqOperator) |
      "!=" ^^ (_ => NotEqOperator) |
      "<=" ^^ (_ => LessOrEqOperator) |
      "<" ^^ (_ => LessOperator) |
      ">=" ^^ (_ => GreaterOrEqOperator) |
      ">" ^^ (_ => GreaterOperator)

  def current: Parser[PathToken] = "@" ^^ (_ => CurrentNode)

  def subQuery: Parser[SubQuery] =
    (current | root) ~ pathSequence ^^ { case c ~ ps => SubQuery(c :: ps) }

  def expression1: Parser[FilterToken] =
    subQuery ~ (comparisonOperator ~ (subQuery | value)).? ^^ {
      case subq1 ~ None         => HasFilter(subq1)
      case lhs ~ Some(op ~ rhs) => ComparisonFilter(op, lhs, rhs)
    }

  def expression2: Parser[FilterToken] =
    value ~ comparisonOperator ~ subQuery ^^ {
      case lhs ~ op ~ rhs => ComparisonFilter(op, lhs, rhs)
    }

  def expression: Parser[FilterToken] = expression1 | expression2

  def booleanOperator: Parser[BinaryBooleanOperator] = "&&" ^^ (_ => AndOperator) | "||" ^^ (_ => OrOperator)

  def booleanExpression: Parser[FilterToken] =
    expression ~ (booleanOperator ~ booleanExpression).? ^^ {
      case lhs ~ None => lhs
      // Balance the AST tree so that all "Or" operations are always on top of any "And" operation. 
      // Indeed, the "And" operations have a higher priority and must be executed first.
      case lhs1 ~ Some(AndOperator ~ BooleanFilter(OrOperator, lhs2, rhs2)) =>
        BooleanFilter(OrOperator, BooleanFilter(AndOperator, lhs1, lhs2), rhs2)
      case lhs ~ Some(op ~ rhs) => BooleanFilter(op, lhs, rhs)
    }

  def recursiveSubscriptFilter: Parser[RecursiveFilterToken] =
    (("..*" | "..") ~> subscriptFilter) ^^ RecursiveFilterToken

  def subscriptFilter: Parser[FilterToken] =
    "[?(" ~> booleanExpression <~ ")]"

  /// child accessors parsers ///////////////////////////////////////////////

  def subscriptField: Parser[FieldAccessor] =
    "[" ~> repsep(quotedField, ",") <~ "]" ^^ {
      case f1 :: Nil => Field(f1)
      case fields    => MultiField(fields)
    }

  def dotField: Parser[FieldAccessor] =
    "." ~> field ^^ Field

  // TODO recursive with `subscriptField`
  def recursiveField: Parser[FieldAccessor] =
    ".." ~> field ^^ RecursiveField

  def anyChild: Parser[FieldAccessor] = (".*" | "['*']" | """["*"]""") ^^ (_ => AnyField)

  def recursiveAny: Parser[FieldAccessor] = "..*" ^^ (_ => RecursiveAnyField)

  def fieldAccessors = (
    dotField
    | recursiveSubscriptFilter
    | recursiveAny
    | recursiveField
    | anyChild
    | subscriptField
  )

  /// Main parsers //////////////////////////////////////////////////////////

  def childAccess = fieldAccessors | arrayAccessors

  def pathSequence: Parser[List[PathToken]] = rep(childAccess | subscriptFilter)

  def root: Parser[PathToken] = "$" ^^ (_ => RootNode)

  def query: Parser[List[PathToken]] =
    phrase(root ~ pathSequence) ^^ { case r ~ ps => r :: ps }
}

class Parser {
  val query = Parser.query
  def compile(jsonpath: String): Parser.ParseResult[List[PathToken]] = Parser.parse(query, jsonpath)
}

object Parser extends ParserBase
