package io.gatling.jsonpath.parboiled

import org.parboiled2._
import shapeless._
import io.gatling.jsonpath.AST._
import scala.annotation.tailrec

class FastParser(val input: ParserInput) extends Parser {

	def string = rule { "\"" ~ capture(zeroOrMore(char)) ~> (JPString(_)) ~ "\"" }

	def char = rule { "a" - "z" | "A" - "Z" | digit }

	def number = rule { capture(integer) ~> (i ⇒ JPLong(i.toInt)) }

	def integer = rule { optional("-") ~ (("1" - "9") ~ zeroOrMore(digit) | oneOrMore(digit)) }

	def digit = rule { "0" - "9" }

	///// 
	def field = rule { capture(zeroOrMore(char)) ~> (Field(_)) }
	def dotField = rule { "." ~ field }

	def dotAnyChild = rule { "*" ~ push(AnyField) }
	def dotRecursiveAnyChild = rule { ".*" ~ push(RecursiveAnyField) }
	def dotMembers = rule { "." ~ (dotAnyChild | dotRecursiveAnyChild | field) }

	def arrayMembers = rule { "['*']" ~ push(AnyField) }

	def childAccess = rule { dotMembers | arrayMembers }

	//def pathSequence: Parser[List[PathToken]] = rep(childAccess | subscriptFilter)
	def pathSequence = rule { zeroOrMore(childAccess) }

	def root = rule { "$" ~ push(RootNode) }

	def query = rule { root ~ pathSequence ~> ((h, t) => h +: t) ~ EOI }
}

object FastParser {

	def parse(jpQuery: String) =
		new FastParser(jpQuery).run(_.query) match {
			case Right(v :: HNil) => Right(v)
			case Left(err) => Left(s"Expression is not valid. Error: ${ErrorUtils.formatError(jpQuery, err)}")
			case _ => Left("OOPs") /// TODO 
		}
}

object SimpleJsonPath {
	@tailrec
	def repl(): Unit = {
		val inputLine = readLine("--------------------------------------\nEnter a JSONPath expression > ")
		if (inputLine != "") {
			val simpleCalc = new FastParser(inputLine)
			val result = simpleCalc.run(_.query)
			result match {
				case Right(x) ⇒ println(s"Expression is valid. Result: ${x}")
				case Left(err) ⇒ println(s"Expression is not valid. Error: ${ErrorUtils.formatError(inputLine, err)}")
			}
			repl()
		}
	}

	def main(args: Array[String]): Unit = {
		repl()
	}
}