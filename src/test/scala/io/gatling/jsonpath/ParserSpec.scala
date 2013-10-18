package io.gatling.jsonpath

import org.scalatest.FlatSpec
import org.scalatest.matchers.{ Matcher, MatchResult }
import io.gatling.jsonpath.Parser._
import org.scalatest.Matchers

class ParserSpec extends FlatSpec with Matchers with ParsingMatchers {

	"Field parsing" should "work with standard names" in {
		def shouldParseField(name: String) = {
			val field = Field(name, false)
			parse(dotField, s".$name") should beParsedAs(field)
			parse(subscriptField, s"['$name']") should beParsedAs(field)
		}

		shouldParseField("foo")
		shouldParseField("$foo")
		shouldParseField("Foo$1bar")
		shouldParseField("_1-2-3")
		shouldParseField("ñ1çölå$")
	}

	it should "work with the root object" in {
		parse(Parser.root, "$") should beParsedAs(RootNode())
	}

	it should "work when having multiple fields" in {
		parse(subscriptField, "['foo', 'bar', 'baz']") should beParsedAs(MultiField(List("foo", "bar", "baz")))
		parse(subscriptField, "['a', 'b c', 'd.e']") should beParsedAs(MultiField(List("a", "b c", "d.e")))
	}

	"Array parsing" should "work with random array accessors" in {
		parse(arrayAccessors, "[1]") should beParsedAs(ArrayRandomAccess(1 :: Nil))
		parse(arrayAccessors, "[42]") should beParsedAs(ArrayRandomAccess(42 :: Nil))
		parse(arrayAccessors, "[-1]") should beParsedAs(ArrayRandomAccess(-1 :: Nil))
		parse(arrayAccessors, "[-1,42 , -9]") should beParsedAs(ArrayRandomAccess(-1 :: 42 :: -9 :: Nil))
	}

	it should "should parse correctly the array slice operator" in {
		// 1 separator
		parse(arrayAccessors, "[:]") should beParsedAs(ArraySlice(None, None, 1))
		parse(arrayAccessors, "[2:]") should beParsedAs(ArraySlice(Some(2), None, 1))
		parse(arrayAccessors, "[:2]") should beParsedAs(ArraySlice(None, Some(2)))
		parse(arrayAccessors, "[2:4]") should beParsedAs(ArraySlice(Some(2), Some(4)))

		// 2 separators
		parse(arrayAccessors, "[::]") should beParsedAs(ArraySlice(None, None, 1))
		parse(arrayAccessors, "[::2]") should beParsedAs(ArraySlice(None, None, 2))
		parse(arrayAccessors, "[2::]") should beParsedAs(ArraySlice(Some(2), None, 1))
		parse(arrayAccessors, "[4::2]") should beParsedAs(ArraySlice(Some(4), None, 2))
		parse(arrayAccessors, "[:4:2]") should beParsedAs(ArraySlice(None, Some(4), 2))
		parse(arrayAccessors, "[0:8:2]") should beParsedAs(ArraySlice(Some(0), Some(8), 2))
	}

	it should "work with array access on the root object" in {
		new Parser().compile("$[1]").get should be(RootNode() :: ArrayRandomAccess(List(1)) :: Nil)
		new Parser().compile("$[*]").get should be(RootNode() :: ArraySlice(None, None) :: Nil)
	}

	it should "work with array access on fields" in {
		parse(pathSequence, ".foo[1]").get should be(Field("foo", false) :: ArrayRandomAccess(List(1)) :: Nil)
		parse(pathSequence, ".ñ1çölå$[*]").get should be(Field("ñ1çölå$", false) :: ArraySlice(None, None) :: Nil)
	}

	it should "work with array access on subscript fields" in {
		parse(pathSequence, "['foo'][1]").get should be(Field("foo", false) :: ArrayRandomAccess(List(1)) :: Nil)
		parse(pathSequence, "['ñ1çölå$'][*]").get should be(Field("ñ1çölå$", false) :: ArraySlice(None, None) :: Nil)
	}

	"Dot fields" should "get parsed properly" in {
		parse(dotField, ".foo") should beParsedAs(Field("foo", false))
		parse(dotField, ".ñ1çölå$") should beParsedAs(Field("ñ1çölå$", false))
	}

	it should "work on the root element" in {
		new Parser().compile("$.foo").get should be(RootNode() :: Field("foo", false) :: Nil)
		new Parser().compile("$['foo']").get should be(RootNode() :: Field("foo", false) :: Nil)

		// TODO  : how to access childs w/ ['xxx'] notation
		new Parser().compile("$..foo").get should be(RootNode() :: Field("foo", true) :: Nil)
	}

	// cf : http://goessner.net/articles/JsonPath
	"Expressions from Goessner specs" should "be correctly parsed" in {
		def shouldParse(query: String, expected: Any) = {
			new Parser().compile(query).get should be(expected)
		}

		shouldParse("$.store.book[0].title", List(
			RootNode(),
			Field("store", false),
			Field("book", false), ArrayRandomAccess(List(0)),
			Field("title", false)))
		shouldParse("$['store']['book'][0]['title']", List(
			RootNode(),
			Field("store", false),
			Field("book", false), ArrayRandomAccess(List(0)),
			Field("title", false)))
		shouldParse("$.store.book[*].author", List(
			RootNode(),
			Field("store", false),
			Field("book", false), ArraySlice(None, None),
			Field("author", false)))
		shouldParse("$..author", List(RootNode(), Field("author", true)))
		shouldParse("$.store.*", List(RootNode(), Field("store", false), AnyField(false)))
		shouldParse("$.store..price", List(RootNode(), Field("store", false), Field("price", true)))
		shouldParse("$..*", List(RootNode(), AnyField(true)))
		shouldParse("$.*", List(RootNode(), AnyField(false)))
		shouldParse("$..book[2]", List(RootNode(), Field("book", true), ArrayRandomAccess(List(2))))
		shouldParse("$.book[*]", List(RootNode(), Field("book", false), ArraySlice(None, None)))
		shouldParse("$..book[*]", List(RootNode(), Field("book", true), ArraySlice(None, None)))
		shouldParse("$.store['store']..book['book'][0].title..title['title'].*..*.book[*]..book[*]", List(
			RootNode(),
			Field("store", false),
			Field("store", false),
			Field("book", true),
			Field("book", false), ArrayRandomAccess(List(0)),
			Field("title", false),
			Field("title", true),
			Field("title", false),
			AnyField(false),
			AnyField(true),
			Field("book", false), ArraySlice(None, None),
			Field("book", true), ArraySlice(None, None)))
	}

	"Failures" should "be handled gracefully" in {
		def gracefulFailure(query: String) =
			new Parser().compile(query) match {
				case Parser.Failure(msg, _) =>
					info(s"""that's an expected failure for "$query": $msg""")
				case other =>
					fail(s"""a Failure was expected but instead, for "$query" got: $other""")
			}

		gracefulFailure("")
		gracefulFailure("foo")
		gracefulFailure("$f")
		gracefulFailure("$.42foo")
		gracefulFailure("$.[42]")
		gracefulFailure("$.[1:2,3]")
		gracefulFailure("$.[?(@.foo && 2)]")
	}

	"Filters" should "work with subqueries" in {
		parse(subscriptFilter, "[?(@..foo)]") should beParsedAs(
			HasFilter(SubQuery(List(CurrentNode(), Field("foo", true)))))
		parse(subscriptFilter, "[?(@.foo.baz)]") should beParsedAs(
			HasFilter(SubQuery(List(CurrentNode(), Field("foo", false), Field("baz", false)))))
		parse(subscriptFilter, "[?(@['foo'])]") should beParsedAs(
			HasFilter(SubQuery(List(CurrentNode(), Field("foo", false)))))

		new Parser().compile("$.things[?(@.foo.bar)]").get should be(RootNode()
			:: Field("things")
			:: HasFilter(SubQuery(CurrentNode() :: Field("foo") :: Field("bar") :: Nil))
			:: Nil)

	}

	it should "work with some predefined comparison operators" in {

		// Check all supported ordering operators
		parse(subscriptFilter, "[?(@ == 2)]") should beParsedAs(
			ComparisonFilter(EqOperator, SubQuery(List(CurrentNode())), JPLong(2)))
		parse(subscriptFilter, "[?(@ <= 2)]") should beParsedAs(
			ComparisonFilter(LessOrEqOperator, SubQuery(List(CurrentNode())), JPLong(2)))
		parse(subscriptFilter, "[?(@ >= 2)]") should beParsedAs(
			ComparisonFilter(GreaterOrEqOperator, SubQuery(List(CurrentNode())), JPLong(2)))
		parse(subscriptFilter, "[?(@ < 2)]") should beParsedAs(
			ComparisonFilter(LessOperator, SubQuery(List(CurrentNode())), JPLong(2)))
		parse(subscriptFilter, "[?(@ > 2)]") should beParsedAs(
			ComparisonFilter(GreaterOperator, SubQuery(List(CurrentNode())), JPLong(2)))

		// Trickier Json path expressions
		parse(subscriptFilter, "[?(@.foo == 2)]") should beParsedAs(
			ComparisonFilter(EqOperator, SubQuery(List(CurrentNode(), Field("foo", false))), JPLong(2)))
		parse(subscriptFilter, "[?(2 == @['foo'])]") should beParsedAs(
			ComparisonFilter(EqOperator, JPLong(2), SubQuery(List(CurrentNode(), Field("foo", false)))))

		// Allow reference to the root object
		parse(subscriptFilter, "[?(@ == $['foo'])]") should beParsedAs(
			ComparisonFilter(EqOperator, SubQuery(List(CurrentNode())), SubQuery(List(RootNode(), Field("foo", false)))))

		new Parser().compile("$['points'][?(@['y'] >= 3)].id").get should be(RootNode()
			:: Field("points", false)
			:: ComparisonFilter(GreaterOrEqOperator, SubQuery(List(CurrentNode(), Field("y", false))), JPLong(3))
			:: Field("id", false) :: Nil)

		new Parser().compile("$.points[?(@['id']=='i4')].x").get should be(RootNode()
			:: Field("points", false)
			:: ComparisonFilter(EqOperator, SubQuery(List(CurrentNode(), Field("id", false))), JPString("i4"))
			:: Field("x", false) :: Nil)
	}

	it should "work with some predefined boolean operators" in {
		parse(subscriptFilter, "[?(@.foo && @.bar)]") should beParsedAs(
			BooleanFilter(AndOperator,
				HasFilter(SubQuery(List(CurrentNode(), Field("foo", false)))),
				HasFilter(SubQuery(List(CurrentNode(), Field("bar", false))))))

		parse(subscriptFilter, "[?(@.foo || @.bar)]") should beParsedAs(
			BooleanFilter(OrOperator,
				HasFilter(SubQuery(List(CurrentNode(), Field("foo", false)))),
				HasFilter(SubQuery(List(CurrentNode(), Field("bar", false))))))

		parse(subscriptFilter, "[?(@.foo || @.bar <= 2)]") should beParsedAs(
			BooleanFilter(OrOperator,
				HasFilter(SubQuery(List(CurrentNode(), Field("foo", false)))),
				ComparisonFilter(LessOrEqOperator, SubQuery(List(CurrentNode(), Field("bar", false))), JPLong(2))))
	}

}

trait ParsingMatchers {

	class SuccessBeMatcher[+T <: AstToken](expected: T) extends Matcher[Parser.ParseResult[AstToken]] {
		def apply(left: Parser.ParseResult[AstToken]): MatchResult = {
			left match {
				case Parser.Success(res, _) => MatchResult(expected == res,
					s"$res is not equal to expected value $expected",
					s"$res is equal to $expected but it shouldn't be")
				case Parser.NoSuccess(msg, _) => MatchResult(false,
					s"parsing issue, $msg",
					s"parsing issue, $msg")
			}
		}
	}

	def beParsedAs[T <: AstToken](expected: T) = new SuccessBeMatcher(expected)
}