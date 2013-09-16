package io.gatling.jsonpath

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult
import io.gatling.jsonpath.Parser._

class ParserSpec extends FlatSpec with ShouldMatchers with ParsingMatchers {

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
		parse(Parser.root, "$") should beParsedAs(Root())
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
		compile("$[1]").get should be(Root() :: ArrayRandomAccess(List(1)) :: Nil)
		compile("$[*]").get should be(Root() :: ArraySlice(None, None) :: Nil)
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
		compile("$.foo").get should be(Root() :: Field("foo", false) :: Nil)
		compile("$['foo']").get should be(Root() :: Field("foo", false) :: Nil)

		// TODO  : how to access childs w/ ['xxx'] notation
		compile("$..foo").get should be(Root() :: Field("foo", true) :: Nil)
	}

	// cf : http://goessner.net/articles/JsonPath
	"Expressions from Goessner specs" should "be correctly parsed" in {
		def shouldParse(query: String, expected: Any) = {
			compile(query).get should be(expected)
		}

		shouldParse("$.store.book[0].title", List(
			Root(),
			Field("store", false),
			Field("book", false), ArrayRandomAccess(List(0)),
			Field("title", false)))
		shouldParse("$['store']['book'][0]['title']", List(
			Root(),
			Field("store", false),
			Field("book", false), ArrayRandomAccess(List(0)),
			Field("title", false)))
		shouldParse("$.store.book[*].author", List(
			Root(),
			Field("store", false),
			Field("book", false), ArraySlice(None, None),
			Field("author", false)))
		shouldParse("$..author", List(Root(), Field("author", true)))
		shouldParse("$.store.*", List(Root(), Field("store", false), AnyField(false)))
		shouldParse("$.store..price", List(Root(), Field("store", false), Field("price", true)))
		shouldParse("$..*", List(Root(), AnyField(true)))
		shouldParse("$.*", List(Root(), AnyField(false)))
		shouldParse("$..book[2]", List(Root(), Field("book", true), ArrayRandomAccess(List(2))))
		shouldParse("$.book[*]", List(Root(), Field("book", false), ArraySlice(None, None)))
		shouldParse("$..book[*]", List(Root(), Field("book", true), ArraySlice(None, None)))
		shouldParse("$.store['store']..book['book'][0].title..title['title'].*..*.book[*]..book[*]", List(
			Root(),
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
			compile(query) match {
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
			HasFilter(SubQuery(List(CurrentObject(), Field("foo", true)))))
		parse(subscriptFilter, "[?(@.foo.baz)]") should beParsedAs(
			HasFilter(SubQuery(List(CurrentObject(), Field("foo", false), Field("baz", false)))))
		parse(subscriptFilter, "[?(@['foo'])]") should beParsedAs(
			HasFilter(SubQuery(List(CurrentObject(), Field("foo", false)))))

		compile("$.things[?(@.foo.bar)]").get should be(Root()
			:: Field("things")
			:: HasFilter(SubQuery(CurrentObject() :: Field("foo") :: Field("bar") :: Nil))
			:: Nil)

	}

	it should "work with some predefined comparison operators" in {

		// Check all supported ordering operators
		parse(subscriptFilter, "[?(@ == 2)]") should beParsedAs(
			ComparisonFilter(EqOperation, SubQuery(List(CurrentObject())), JPLong(2)))
		parse(subscriptFilter, "[?(@ <= 2)]") should beParsedAs(
			ComparisonFilter(LessOrEqOperation, SubQuery(List(CurrentObject())), JPLong(2)))
		parse(subscriptFilter, "[?(@ >= 2)]") should beParsedAs(
			ComparisonFilter(GreaterOrEqOperation, SubQuery(List(CurrentObject())), JPLong(2)))
		parse(subscriptFilter, "[?(@ < 2)]") should beParsedAs(
			ComparisonFilter(LessOperation, SubQuery(List(CurrentObject())), JPLong(2)))
		parse(subscriptFilter, "[?(@ > 2)]") should beParsedAs(
			ComparisonFilter(GreaterOperation, SubQuery(List(CurrentObject())), JPLong(2)))

		// Trickier Json path expressions
		parse(subscriptFilter, "[?(@.foo == 2)]") should beParsedAs(
			ComparisonFilter(EqOperation, SubQuery(List(CurrentObject(), Field("foo", false))), JPLong(2)))
		parse(subscriptFilter, "[?(2 == @['foo'])]") should beParsedAs(
			ComparisonFilter(EqOperation, JPLong(2), SubQuery(List(CurrentObject(), Field("foo", false)))))

		compile("$['points'][?(@['y'] >= 3)].id").get should be(Root()
			:: Field("points", false)
			:: ComparisonFilter(GreaterOrEqOperation, SubQuery(List(CurrentObject(), Field("y", false))), JPLong(3))
			:: Field("id", false) :: Nil)

		compile("$.points[?(@['id']=='i4')].x").get should be(Root()
			:: Field("points", false)
			:: ComparisonFilter(EqOperation, SubQuery(List(CurrentObject(), Field("id", false))), JPString("i4"))
			:: Field("x", false) :: Nil)
	}

	it should "work with some predefined boolean operators" in {
		parse(subscriptFilter, "[?(@.foo && @.bar)]") should beParsedAs(
			BooleanFilter(AndOperation,
				HasFilter(SubQuery(List(CurrentObject(), Field("foo", false)))),
				HasFilter(SubQuery(List(CurrentObject(), Field("bar", false))))))
		
		parse(subscriptFilter, "[?(@.foo || @.bar)]") should beParsedAs(
			BooleanFilter(OrOperation,
				HasFilter(SubQuery(List(CurrentObject(), Field("foo", false)))),
				HasFilter(SubQuery(List(CurrentObject(), Field("bar", false))))))
		
			parse(subscriptFilter, "[?(@.foo || @.bar <= 2)]") should beParsedAs(
			BooleanFilter(OrOperation,
				HasFilter(SubQuery(List(CurrentObject(), Field("foo", false)))),
				ComparisonFilter(LessOrEqOperation, SubQuery(List(CurrentObject(), Field("bar", false))), JPLong(2))))
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