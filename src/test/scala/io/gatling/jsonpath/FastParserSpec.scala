package io.gatling.jsonpath

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import io.gatling.jsonpath.parboiled.FastParser.parse
import io.gatling.jsonpath.AST._

class FastParserSpec extends FlatSpec with Matchers {

	"Nominal queries" should "start with root element" in {
		parse("$") should be(Right(Seq(RootNode)))
		parse("£") should be('left)
	}

	it should "be able to access fields" in {
		parse("$.foo") should be(Right(Seq(RootNode, Field("foo"))))
	}

	it should "be able to select any fields" in {
		parse("$.*") should be(Right(Seq(RootNode, AnyField)))
		parse("$.foo.*") should be(Right(Seq(RootNode, Field("foo"), AnyField)))
		parse("$.foo[*]") should be(Right(Seq(RootNode, Field("foo"), AnyField)))

	}

	it should "be able to access fields recursively" in {
		parse("$..foo") should be(Right(Seq(RootNode, RecursiveField("foo"))))

		parse("$..*") should be(Right(Seq(RootNode, RecursiveAnyField)))
		parse("$.foo..*") should be(Right(Seq(RootNode, Field("foo"), RecursiveAnyField)))
	}
}