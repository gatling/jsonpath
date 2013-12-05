package io.gatling.jsonpath

import scala.annotation.switch
import scala.util.parsing.combinator._
import io.gatling.jsonpath.AST._


object Parser extends RegexParsers {

	val numberRegex = """-?\d+""".r
	val fieldRegex = """[$_\p{L}][$_\-\d\p{L}]*""".r
	val quotedFieldRegex = "[^']+".r
	val numberValueRegex = """-?\d+(\.\d*)?""".r
	val comparisonOperatorRegex = "==|<=|>=|<|>".r
	val booleanOperatorRegex = """\|\||&&""".r

	/// general purpose parsers ///////////////////////////////////////////////

	def number: Parser[Int] = numberRegex ^^ (_.toInt)

	def field: Parser[String] = fieldRegex

	def quotedField: Parser[String] = "'" ~> quotedFieldRegex <~ "'"

	/// array parsers /////////////////////////////////////////////////////////

	def arraySliceStep: Parser[Option[Int]] = ":" ~> number.?

	def arraySlice: Parser[ArraySlice] =
		(":" ~> number.?) ~ arraySliceStep.? ^^ {
			case end ~ step => ArraySlice(None, end, step.flatten.getOrElse(1))
		}

	def arrayRandomAccess: Parser[Option[ArrayRandomAccess]] =
		rep("," ~> number).? ^^ (indices => indices.map(ArrayRandomAccess))

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

	def parseComparisonOperator(op: String) = (op.charAt(0): @switch) match {
		case '=' => EqOperator
		case '<' => if (op.size == 1) LessOperator else LessOrEqOperator
		case '>' => if (op.size == 1) GreaterOperator else GreaterOrEqOperator
	}

	def numberValue: Parser[JPNumber] = numberValueRegex ^^ {
		s => if (s.indexOf('.') != -1) JPDouble(s.toDouble) else JPLong(s.toLong)
	}
	def stringValue: Parser[JPString] = quotedField ^^ { JPString }
	def value: Parser[FilterValue] = (numberValue | stringValue)

	def comparisonOperator: Parser[String] = comparisonOperatorRegex

	def current: Parser[PathToken] = "@" ^^ (_ => CurrentNode)

	def subQuery: Parser[SubQuery] =
		(current | root) ~ pathSequence ^^ { case c ~ ps => SubQuery(c :: ps) }

	def expression1: Parser[FilterToken] =
		subQuery ~ (comparisonOperator ~ (subQuery | value)).? ^^ {
			case subq1 ~ None => HasFilter(subq1)
			case lhs ~ Some(op ~ rhs) => ComparisonFilter(parseComparisonOperator(op), lhs, rhs)
		}

	def expression2: Parser[FilterToken] =
		value ~ comparisonOperator ~ (subQuery | value) ^^ {
			case lhs ~ op ~ rhs => ComparisonFilter(parseComparisonOperator(op), lhs, rhs)
		}

	def expression: Parser[FilterToken] = expression1 | expression2

	def booleanOperator: Parser[String] = booleanOperatorRegex

	def parseBooleanOperator(op: String) = (op.charAt(0): @switch) match {
		case '&' => AndOperator
		case '|' => OrOperator
	}

	def booleanExpression: Parser[FilterToken] =
		expression ~ (booleanOperator ~ expression).? ^^ {
			case lhs ~ None => lhs
			case lhs ~ Some(op ~ rhs) => BooleanFilter(parseBooleanOperator(op), lhs, rhs)
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

	def anyChild: Parser[FieldAccessor] = (".*" | "['*']") ^^ (_ => AnyField)

	def anyRecursive: Parser[FieldAccessor] = "..*" ^^ (_ => RecursiveAnyField)

	def fieldAccessors = (
		dotField
		| anyRecursive
		| anyChild
		| recursiveField
		| subscriptField)

	/// Main parsers //////////////////////////////////////////////////////////

	def childAccess = (fieldAccessors | arrayAccessors)

	def pathSequence: Parser[List[PathToken]] = rep(childAccess | subscriptFilter)

	def root: Parser[PathToken] = "$" ^^ (_ => RootNode)

	def query: Parser[List[PathToken]] =
		phrase(root ~ pathSequence) ^^ { case r ~ ps => r :: ps }
}

class Parser {
	val query = Parser.query
	def compile(jsonpath: String): Parser.ParseResult[List[PathToken]] = Parser.parse(query, jsonpath)
}
