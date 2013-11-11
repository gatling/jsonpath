package io.gatling.jsonpath

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import io.gatling.jsonpath.parboiled.FastParser.parse
import io.gatling.jsonpath.AST._

class FastParserSpec extends FlatSpec with Matchers {

	"Field parsing" should "work with standard names" in {

		parse("$") should be(Right(Seq(RootNode)))
		parse("$.foo") should be(Right(Seq(RootNode, Field("foo"))))

		parse("$.*") should be(Right(Seq(RootNode, AnyField)))
		parse("$.foo.*") should be(Right(Seq(RootNode, Field("foo"), AnyField)))
		parse("$.foo['*']") should be(Right(Seq(RootNode, Field("foo"), AnyField)))
	}
}