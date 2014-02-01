/**
 * Copyright 2011-2013 Gatling (gatling.io)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * 		http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.gatling.jsonpath

import scala.util.parsing.combinator.RegexParsers

import io.gatling.jsonpath.AST._

object Parser extends RegexParsers {

	val numberRegex = """-?\d+""".r
	val fieldRegex = """[$_\p{L}][$_\-\d\p{L}]*""".r
	val singleQuotedFieldRegex = "[^']+".r
	val doubleQuotedFieldRegex = """[^"]+""".r
	val numberValueRegex = """-?\d+(\.\d*)?""".r

	/// general purpose parsers ///////////////////////////////////////////////

	def number: Parser[Int] = numberRegex ^^ (_.toInt)

	def field: Parser[String] = fieldRegex

	def quotedField: Parser[String] = ("'" ~> singleQuotedFieldRegex <~ "'") | ("\"" ~> doubleQuotedFieldRegex <~ "\"")

	/// array parsers /////////////////////////////////////////////////////////

	def arraySliceStep: Parser[Option[Int]] = ":" ~> number.?

	def arraySlice: Parser[ArraySlice] =
		(":" ~> number.?) ~ arraySliceStep.? ^^ {
			case end ~ step => ArraySlice(None, end, step.flatten.getOrElse(1))
		}

	def arrayRandomAccess: Parser[Option[ArrayRandomAccess]] =
		rep1("," ~> number).? ^^ (indices => indices.map(ArrayRandomAccess))

	def arrayPartial: Parser[ArrayAccessor] =
		number ~ (arraySlice | arrayRandomAccess) ^^ {
			case i ~ None => ArrayRandomAccess(i :: Nil)
			case i ~ Some(ArrayRandomAccess(indices)) => ArrayRandomAccess(i :: indices)
			case i ~ (as @ ArraySlice(_, _, _)) => as.copy(start = Some(i))
		}

	def arrayAll: Parser[ArraySlice] =
		"*" ^^ (_ => ArraySlice(None, None))

	def arrayAccessors: Parser[ArrayAccessor] =
		"[" ~> (arrayAll | arrayPartial | arraySlice) <~ "]"

	/// filters parsers ///////////////////////////////////////////////////////

	def numberValue: Parser[JPNumber] = numberValueRegex ^^ {
		s => if (s.indexOf('.') != -1) JPDouble(s.toDouble) else JPLong(s.toLong)
	}
	def booleanValue: Parser[JPBoolean] = 
		"true" ^^ (_ => JPBoolean(true)) |
		"false" ^^ (_ => JPBoolean(false))
	
	def stringValue: Parser[JPString] = quotedField ^^ { JPString }
	def value: Parser[FilterValue] = (booleanValue | numberValue | stringValue)

	def comparisonOperator: Parser[ComparisonOperator] =
		"==" ^^ (_ => EqOperator) |
			"<=" ^^ (_ => LessOrEqOperator) |
			"<" ^^ (_ => LessOperator) |
			">=" ^^ (_ => GreaterOrEqOperator) |
			">" ^^ (_ => GreaterOperator)

	def current: Parser[PathToken] = "@" ^^ (_ => CurrentNode)

	def subQuery: Parser[SubQuery] =
		(current | root) ~ pathSequence ^^ { case c ~ ps => SubQuery(c :: ps) }

	def expression1: Parser[FilterToken] =
		subQuery ~ (comparisonOperator ~ (subQuery | value)).? ^^ {
			case subq1 ~ None => HasFilter(subq1)
			case lhs ~ Some(op ~ rhs) => ComparisonFilter(op, lhs, rhs)
		}

	def expression2: Parser[FilterToken] =
		value ~ comparisonOperator ~ subQuery ^^ {
			case lhs ~ op ~ rhs => ComparisonFilter(op, lhs, rhs)
		}

	def expression: Parser[FilterToken] = expression1 | expression2

	def booleanOperator: Parser[BinaryBooleanOperator] = "&&" ^^ (_ => AndOperator) | "||" ^^ (_ => OrOperator)

	def booleanExpression: Parser[FilterToken] =
		expression ~ (booleanOperator ~ expression).? ^^ {
			case lhs ~ None => lhs
			case lhs ~ Some(op ~ rhs) => BooleanFilter(op, lhs, rhs)
		}

	def subscriptFilter: Parser[PathToken] =
		"[?(" ~> booleanExpression <~ ")]"

	/// child accessors parsers ///////////////////////////////////////////////

	def subscriptField: Parser[FieldAccessor] =
		"[" ~> repsep(quotedField, ",") <~ "]" ^^ {
			case f1 :: Nil => Field(f1)
			case fields => MultiField(fields)
		}

	def dotField: Parser[FieldAccessor] =
		"." ~> field ^^ Field

	// TODO recursive with `subscriptField`
	def recursiveField: Parser[FieldAccessor] =
		".." ~> field ^^ RecursiveField

	def anyChild: Parser[FieldAccessor] = (".*" | "['*']" | """["*"]""") ^^ (_ => AnyField)

	def anyRecursive: Parser[FieldAccessor] = "..*" ^^ (_ => RecursiveAnyField)

	def fieldAccessors = (
		dotField
		| anyRecursive
		| anyChild
		| recursiveField
		| subscriptField)

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
