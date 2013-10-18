package io.gatling.jsonpath

import scala.util.parsing.combinator._
import io.gatling.jsonpath.AST._

object Parser extends RegexParsers {

	/// general purpose parsers ///////////////////////////////////////////////

	def number: Parser[Int] = """-?\d+""".r ^^ (_.toInt)

	def field: Parser[String] = """[$_\p{L}][$_\-\d\p{L}]*""".r

	def quotedField: Parser[String] = "'" ~> "[^']+".r <~ "'"

	/// array parsers /////////////////////////////////////////////////////////

	def arraySliceStep: Parser[Option[Int]] = ":" ~> number.?

	def arraySlice: Parser[ArraySlice] =
		(":" ~> number.?) ~ arraySliceStep.? ^^ {
			case end ~ step => ArraySlice(None, end, step.flatten.getOrElse(1))
		}

	def arrayRandomAccess: Parser[Option[ArrayRandomAccess]] =
		rep("," ~> number).? ^^ (indices => indices.map(ArrayRandomAccess(_)))

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

	def parseComparisonOperator(op: String) = op match {
		case "==" => EqOperator
		case "<" => LessOperator
		case ">" => GreaterOperator
		case "<=" => LessOrEqOperator
		case ">=" => GreaterOrEqOperator
	}

	def numberValue: Parser[JPNumber] = """-?\d+(\.\d*)?""".r ^^ {
		s => if (s.contains(".")) JPDouble(s.toDouble) else JPLong(s.toLong)
	}
	def stringValue: Parser[JPString] = quotedField ^^ { JPString(_) }
	def value: Parser[FilterValue] = (numberValue | stringValue)

	def comparisonOperator: Parser[String] = "==|<=|>=|<|>".r

	def current: Parser[PathToken] = "@" ^^ (_ => currentNode)

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

	def booleanOperator: Parser[String] = """\|\||&&""".r

	def parseBooleanOperator(op: String) = op match {
		case "&&" => AndOperator
		case "||" => OrOperator
	}

	def booleanExpression: Parser[FilterToken] =
		expression ~ (booleanOperator ~ expression).? ^^ {
			case lhs ~ None => lhs
			case lhs ~ Some(op ~ rhs) => BooleanFilter(parseBooleanOperator(op), lhs, rhs)
		}

	def subscriptFilter: Parser[PathToken] =
		"[?(" ~> (booleanExpression) <~ ")]"

	/// child accessors parsers ///////////////////////////////////////////////

	def subscriptField: Parser[FieldAccessor] =
		"[" ~> repsep(quotedField, ",") <~ "]" ^^ {
			case f1 :: Nil => Field(f1, false)
			case fields => MultiField(fields)
		}

	def dotField: Parser[FieldAccessor] =
		"." ~> field ^^ (Field(_, false))

	// TODO recursive with `subscriptField`
	def recursiveField: Parser[FieldAccessor] =
		".." ~> field ^^ (Field(_, true))

	def anyChild: Parser[FieldAccessor] = (".*" | "['*']") ^^ (_ => anyField)

	def anyRecursive: Parser[FieldAccessor] = "..*" ^^ (_ => anyRecursiveField)

	def fieldAccessors = (
		dotField
		| anyRecursive
		| anyChild
		| recursiveField
		| subscriptField)

	/// Main parsers //////////////////////////////////////////////////////////

	def childAccess = (fieldAccessors | arrayAccessors)

	def pathSequence: Parser[List[PathToken]] = rep(childAccess | subscriptFilter)

	def root: Parser[PathToken] = "$" ^^ (_ => rootNode)

	def query: Parser[List[PathToken]] =
		phrase(root ~ pathSequence) ^^ { case r ~ ps => r :: ps }
}

class Parser {
	def compile(jsonpath: String): Parser.ParseResult[List[PathToken]] = Parser.parse(Parser.query, jsonpath)
}
