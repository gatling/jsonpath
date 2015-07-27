/**
 * Copyright 2011-2015 eBusiness Information, Groupe Excilys (www.ebusinessinformation.fr)
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

import java.util.{ HashMap => JHashMap, List => JList }

import scala.collection.JavaConversions.seqAsJavaList

import org.scalatest.{ FlatSpec, Matchers }
import org.scalatest.matchers.{ MatchResult, Matcher }

import com.fasterxml.jackson.databind.ObjectMapper

class JsonPathSpec extends FlatSpec with Matchers with JsonPathMatchers {

  val mapper = new ObjectMapper

  def parseJson(s: String) = mapper.readValue(s, classOf[Object])
  def bool(b: Boolean) = b
  def int(i: Int) = i
  def double(f: Double) = f
  def text(s: String) = s
  def nullNode: Any = null
  def array(elts: Any*): JList[Any] = elts
  def obj(elts: (String, Any)*) = elts.foldLeft(new JHashMap[String, Any]())((o: JHashMap[String, Any], e) => {
    o.put(e._1, e._2)
    o
  })

  // Goessner JSON exemple

  val book1 = """{"category":"reference","author":"Nigel Rees","title":"Sayings of the Century","price":8.95}"""
  val book2 = """{"category":"fiction","author":"Evelyn Waugh","title":"Sword of Honour","price":12.99}"""
  val book3 = """{"category":"fiction","author":"Herman Melville","title":"Moby Dick","isbn":"0-553-21311-3","price":8.99}"""
  val book4 = """{"category":"fiction","author":"J. R. R. Tolkien","title":"The Lord of the Rings","isbn":"0-395-19395-8","price":22.99}"""
  val allBooks = s"[$book1,$book2,$book3,$book4]"
  val bicycle = s"""{"color":"red","price":19.95}"""
  val allStore = s"""{"book":$allBooks, "bicycle":$bicycle}"""
  val goessnerData = s"""{"store":$allStore}"""
  val goessnerJson = parseJson(goessnerData)

  val json = """[
               |    {
               |        "id":19434,
               |        "foo":1,
               |        "company":
               |        {
               |            "id":18971
               |        },
               |        "owner":
               |        {
               |            "id":18957
               |        },
               |        "process":
               |        {
               |            "id":18972
               |        }
               |    },
               |    {
               |        "id":19435,
               |        "foo":2,
               |        "company":
               |        {
               |            "id":18972
               |        },
               |        "owner":
               |        {
               |            "id":18957
               |        },
               |        "process":
               |        {
               |            "id":18974
               |        }
               |    }
               |]""".stripMargin

  val json2 = """[
                |  {
                |    "unit": "flow.flow-server.aaa.gatlinguser202project.gatlinguser202.service",
                |    "machine": "f374e9a176b341d4a8ee8db3cdfb2958/10.11.12.124",
                |    "active": "active",
                |    "sub": "running",
                |    "space": "aaa.gatlinguser202project.gatlinguser202",
                |    "@timestamp": 1418285656070
                |  },
                |  {
                |    "unit": "XXXXXXXflow.flow-server.aaa.gatlinguser202project.gatlinguser202.service",
                |    "machine": "f374e9a176b341d4a8ee8db3cdfb2958/10.11.12.124",
                |    "active": "active",
                |    "sub": "running",
                |    "space": "aaa.gatlinguser202project.gatlinguser202",
                |    "@timestamp": 1418285656070
                |  }
                |]""".stripMargin

  private val veggies = """[
                          |    {
                          |        "vegetable": {
                          |            "name": "peas",
                          |            "color": "green"
                          |        },
                          |        "meet": {
                          |            "name":"beef",
                          |            "color":"red"
                          |        }
                          |    },
                          |    {
                          |        "vegetable": {
                          |            "name": "potato",
                          |            "color": "yellow"
                          |        },
                          |        "meet": {
                          |            "name":"lamb",
                          |            "color":"brown"
                          |        }
                          |    }
                          |]""".stripMargin

  //////////////

  "Incorrect JsonPath expressions" should "be handled properly" in {
    JsonPath.query("€.$", goessnerJson) should be('left)
  }

  "Support of Goessner test cases" should "work with test set 1" in {
    val json = parseJson("""{"a":"a","b":"b","c d":"e"}""")
    JsonPath.query("$.a", json) should findElements(text("a"))
    JsonPath.query("$['a']", json) should findElements(text("a"))
    // Not supported syntax "$.'c d'", here is an alternative to it
    JsonPath.query("$['c d']", json) should findElements(text("e"))
    JsonPath.query("$.*", json) should findElements(text("a"), text("b"), text("e"))
    JsonPath.query("$['*']", json) should findElements(text("a"), text("b"), text("e"))
    // Not supported syntax "$[*]" ... shouldn't that operator only apply on arrays ?
  }

  it should "work with test set 2" in {
    val json = parseJson("""[ 1, "2", 3.14, true, null ]""")
    JsonPath.query("$[0]", json) should findOrderedElements(int(1))
    JsonPath.query("$[4]", json) should findOrderedElements(nullNode)
    JsonPath.query("$[*]", json) should findOrderedElements(
      int(1), text("2"), double(3.14), bool(true), nullNode
    )
    JsonPath.query("$[-1:]", json) should findOrderedElements(nullNode)
  }

  it should "work with test set 3" in {
    val json = parseJson("""{ "points": [
    				             { "id":"i1", "x": 4, "y":-5 },
    				             { "id":"i2", "x":-2, "y": 2, "z":1 },
    				             { "id":"i3", "x": 8, "y": 3 },
    				             { "id":"i4", "x":-6, "y":-1 },
    				             { "id":"i5", "x": 0, "y": 2, "z":1 },
    				             { "id":"i6", "x": 1, "y": 4 }
    				           ]
    				         }""")

    JsonPath.query("$.points[1]", json) should findOrderedElements(parseJson("""{ "id":"i2", "x":-2, "y": 2, "z":1 }"""))
    JsonPath.query("$.points[4].x", json) should findOrderedElements(int(0))
    JsonPath.query("$.points[?(@['id']=='i4')].x", json) should findOrderedElements(int(-6))
    JsonPath.query("$.points[*].x", json) should findOrderedElements(int(4), int(-2), int(8), int(-6), int(0), int(1))
    // Non supported syntax "$['points'][?(@['x']*@['x']+@['y']*@['y'] > 50)].id"
    JsonPath.query("$['points'][?(@['y'] >= 3)].id", json) should findOrderedElements(text("i3"), text("i6"))
    JsonPath.query("$.points[?(@['z'])].id", json) should findOrderedElements(text("i2"), text("i5"))
    // Non supported syntax "$.points[(count(@)-1)].id"
  }

  it should "work with boolean filters" in {
    val json = parseJson("""{ "conditions":
    			[true, false, true]
    		}""")

    JsonPath.query("$.conditions[?(@ == true)]", json) should findElements(bool(true), bool(true))
    JsonPath.query("$.conditions[?(@ == false)]", json) should findElements(bool(false))
    JsonPath.query("$.conditions[?(false == @)]", json) should findElements(bool(false))
  }

  it should "work with nested boolean filters" in {
    val json = parseJson("""{ "conditions":
    			[
    				{ "id": "i1", "condition": true },
    				{ "id": "i2", "condition": false }
    			]
    		}""")

    JsonPath.query("$.conditions[?(@['condition'] == true)].id", json) should findElements(text("i1"))
    JsonPath.query("$.conditions[?(@['condition'] == false)].id", json) should findElements(text("i2"))
  }

  "Field accessors" should "work with a simple object" in {
    val json = parseJson("""{"foo" : "bar"}""")
    JsonPath.query("$.*", json) should findElements(text("bar"))
    JsonPath.query("$.foo", json) should findElements(text("bar"))
    JsonPath.query("$..foo", json) should findElements(text("bar"))
    JsonPath.query("$.bar", json) should findElements()
  }

  it should "work with nested objects" in {
    val json = parseJson(""" { "foo" : {"bar" : "baz"} }""")
    val x = JsonPath.query("$.foo", json)
    x should findElements(obj("bar" -> text("baz")))
    JsonPath.query("$.foo.bar", json) should findElements(text("baz"))
    JsonPath.query("$..bar", json) should findElements(text("baz"))
  }

  it should "work with arrays" in {
    val json = parseJson("""{"foo":[{"lang":"en"},{"lang":"fr"}]}""")
    JsonPath.query("$.foo[*].lang", json) should findOrderedElements(text("en"), text("fr"))
  }

  it should "work with null elements" in {
    val json = parseJson("""{"foo":null}""")
    JsonPath.query("$.foo[*]", json) should findElements()
  }

  it should "work with nested arrays" in {
    val json = parseJson("""[[{"foo":1}]]""")
    JsonPath.query("$.foo", json) should findOrderedElements()
    JsonPath.query("$..foo", json) should findOrderedElements(int(1))
    JsonPath.query("$[0][0].foo", json) should findOrderedElements(int(1))
  }

  "Multi-fields accessors" should "be interpreted correctly" in {
    val json = parseJson("""{"menu":{"year":2013,"file":"open","options":[{"bold":true},{"font":"helvetica"},{"size":3}]}}""")
    JsonPath.query("$.menu['file','year']", json) should findElements(text("open"), int(2013))
    JsonPath.query("$..options['foo','bar']", json) should findElements()
    JsonPath.query("$..options[*]['bold','size']", json) should findOrderedElements(bool(true), int(3))
  }

  val ten = parseJson("[1,2,3,4,5,6,7,8,9,10]")

  "Array field slicing" should "work with random accessors" in {
    JsonPath.query("$[0]", goessnerJson) should findElements()
    JsonPath.query("$[0]", ten) should findOrderedElements(int(1))
    JsonPath.query("$[-1]", ten) should findOrderedElements(int(10))
    JsonPath.query("$[9]", ten) should findOrderedElements(int(10))
    JsonPath.query("$[2,7]", ten) should findOrderedElements(int(3), int(8))
    JsonPath.query("$[2,-7]", ten) should findOrderedElements(int(3), int(4))
    JsonPath.query("$[2,45]", ten) should findOrderedElements(int(3))
  }

  it should "work when the slice operator has one separator" in {
    JsonPath.query("$[:-1]", goessnerJson) should findElements()
    JsonPath.query("$[:]", ten) should findOrderedElements(
      int(1), int(2), int(3), int(4), int(5), int(6), int(7), int(8), int(9), int(10)
    )
    JsonPath.query("$[7:]", ten) should findOrderedElements(int(8), int(9), int(10))
    JsonPath.query("$[-2:]", ten) should findOrderedElements(int(9), int(10))
    JsonPath.query("$[:3]", ten) should findOrderedElements(int(1), int(2), int(3))
    JsonPath.query("$[:-7]", ten) should findOrderedElements(int(1), int(2), int(3))
    JsonPath.query("$[3:6]", ten) should findOrderedElements(int(4), int(5), int(6))
    JsonPath.query("$[-5:-2]", ten) should findOrderedElements(int(6), int(7), int(8))
  }

  it should "work when the slice operator has two separators" in {
    JsonPath.query("$[:6:2]", ten) should findOrderedElements(int(1), int(3), int(5))
    JsonPath.query("$[1:9:3]", ten) should findOrderedElements(int(2), int(5), int(8))
    JsonPath.query("$[:5:-1]", ten) should findOrderedElements(int(10), int(9), int(8), int(7))
    JsonPath.query("$[:-4:-1]", ten) should findOrderedElements(int(10), int(9), int(8))
    JsonPath.query("$[3::-1]", ten) should findOrderedElements(int(4), int(3), int(2), int(1))
    JsonPath.query("$[-8::-1]", ten) should findOrderedElements(int(3), int(2), int(1))
  }

  "Filters" should "work with subqueries" in {
    val json = parseJson("""[{"foo":1},{"foo":2},{"bar":3}]""")
    JsonPath.query("$[?(@.foo)]", json) should findOrderedElements(obj("foo" -> int(1)), obj("foo" -> int(2)))

    val json2 = parseJson("""{"all":[{"foo":{"bar":1,"baz":2}},{"foo":3}]}""")
    JsonPath.query("$.all[?(@.foo.bar)]", json2) should findElements(parseJson("""{"foo":{"bar":1,"baz":2}}"""))

    val json3 = parseJson(""" { "foo":{"bar":1, "baz":2}, "second":{"bar":3} }  """)
    JsonPath.query("$[?(@.baz)]", json3) should findElements(parseJson("""{"bar":1,"baz":2}"""))
  }

  it should "work with some boolean operators" in {
    val oneToFive = parseJson("[1,2,3,4,5]")
    JsonPath.query("$[?(@ > 3)]", oneToFive) should findOrderedElements(int(4), int(5))
    JsonPath.query("$[?(@ == 3)]", oneToFive) should findOrderedElements(int(3))

    val json = parseJson("""[{"foo":"a"},{"foo":"b"},{"bar":"c"}]""")
    JsonPath.query("$[?(@.foo=='a' )]", json) should findOrderedElements(obj("foo" -> text("a")))
  }

  it should "work with non-alphanumeric values" in {
    val json = parseJson("""{ "a":[{ "a":5, "@":2, "$":5 },
    						              { "a":6, "@":3, "$":4 },
    						              { "a":7, "@":4, "$":5 }
    						             ]}""")
    JsonPath.query("""$.a[?(@['@']==3)]""", json) should findElements(parseJson("""{"a":6,"@":3,"$":4}"""))
    JsonPath.query("""$.a[?(@['$']!=5)]""", json) should findElements(parseJson("""{"a":6,"@":3,"$":4}"""))
  }

  it should "work with some predefined comparison operators" in {
    val oneToSeven = parseJson("[1,2,3,4,5,6,7]")
    JsonPath.query("$[0][?(@>1)]", oneToSeven) should findElements()
    JsonPath.query("$[?( @>1 && @<=4 )]", oneToSeven) should findOrderedElements(int(2), int(3), int(4))
    JsonPath.query("$[?( @>6 && @<2 || @==3 || @<=4 && @>=4 )]", oneToSeven) should findOrderedElements(int(3), int(4))
    JsonPath.query("$[?( @==7 || @<=4 && @>1)]", oneToSeven) should findOrderedElements(int(2), int(3), int(4), int(7))
    JsonPath.query("$[?( @==1 || @>4 )]", oneToSeven) should findOrderedElements(int(1), int(5), int(6), int(7))
  }

  it should "support reference to the root-node" in {
    val authors = """[{"pseudo":"Tolkien","name": "J. R. R. Tolkien"},{"pseudo":"Hugo","name":"Victor Hugo"}]"""
    val library = parseJson(s"""{"book":$allBooks,"authors":$authors}""")

    JsonPath.query("""$.authors[?(@.pseudo=='Tolkien')].name""", library) should findElements(text("J. R. R. Tolkien"))

    JsonPath.query("""$.book[?(@.author==$.authors[?(@.pseudo=='Tolkien')].name)].title""", library) should findElements(text("The Lord of the Rings"))
    JsonPath.query("""$.book[?(@.author==$.authors[?(@.pseudo=='Hugo')].name)].title""", library) should findElements()
  }

  "`null` elements" should "be correctly handled" in {
    val fooNull = parseJson("""{"foo":null}""")
    JsonPath.query("$.foo", fooNull) should findElements(null)
    JsonPath.query("$.foo.bar", fooNull) should findElements()

    val arrayWithNull = parseJson("""{"foo":[1,null,3,"woot"]}""")
    JsonPath.query("$.foo[?(@==null)]", arrayWithNull) should findElements(null)
    JsonPath.query("$.foo[?(@>=null)]", arrayWithNull) should findElements()
    JsonPath.query("$.foo[?(@>=0.5)]", arrayWithNull) should findOrderedElements(1, 3)
  }

  "empty String value" should "be correctly handled" in {
    val emptyStringValue = parseJson("""[{"foo": "bar", "baz": ""}]""")
    JsonPath.query("$[?(@.baz == '')].foo", emptyStringValue) should findElements(text("bar"))
  }

  /// Goessner reference examples ///////////////////////////////////////////

  "Goessner examples" should "work with finding all the authors" in {

    JsonPath.query("$.store.book[*].author", goessnerJson) should findOrderedElements(
      text("Nigel Rees"), text("Evelyn Waugh"), text("Herman Melville"), text("J. R. R. Tolkien")
    )
    JsonPath.query("$..author", goessnerJson) should findOrderedElements(
      text("Nigel Rees"), text("Evelyn Waugh"), text("Herman Melville"), text("J. R. R. Tolkien")
    )
  }

  it should "work with getting the whole store" in {
    JsonPath.query("$..book.*", goessnerJson) should findElements()
    JsonPath.query("$.store.*", goessnerJson) should findOrderedElements(parseJson(allBooks), parseJson(bicycle))
  }

  it should "work with getting all prices" in {
    JsonPath.query("$.store..price", goessnerJson) should findOrderedElements(
      double(8.95), double(12.99), double(8.99), double(22.99), double(19.95)
    )
  }

  it should "work with getting books by indices" in {
    JsonPath.query("$..book[2]", goessnerJson) should findOrderedElements(parseJson(book3))
    JsonPath.query("$..book[-1:]", goessnerJson) should findOrderedElements(parseJson(book4))
    JsonPath.query("$..book[0,1]", goessnerJson) should findOrderedElements(parseJson(book1), parseJson(book2))
    JsonPath.query("$..book[:2]", goessnerJson) should findOrderedElements(parseJson(book1), parseJson(book2))
  }

  it should "allow to get everything" in {
    JsonPath.query("$..*", goessnerJson) should findElements(goessnerJson, parseJson(allStore),
      parseJson(bicycle), text("red"), double(19.95),
      parseJson(allBooks),
      text("Nigel Rees"), text("Sayings of the Century"), text("reference"), double(8.95),
      text("Evelyn Waugh"), text("Sword of Honour"), text("fiction"), double(12.99),
      text("Herman Melville"), text("Moby Dick"), text("fiction"), double(8.99), text("0-553-21311-3"),
      text("J. R. R. Tolkien"), text("The Lord of the Rings"), text("fiction"), double(22.99), text("0-395-19395-8"))
  }

  it should "work with subscript filters" in {
    JsonPath.query("$..book[?(@.isbn)]", goessnerJson) should findOrderedElements(
      parseJson(book3), parseJson(book4)
    )
    JsonPath.query("$..book[?(@.isbn)].title", goessnerJson) should findOrderedElements(
      text("Moby Dick"), text("The Lord of the Rings")
    )
    JsonPath.query("$.store.book[?(@.category == 'fiction')].title", goessnerJson) should findOrderedElements(
      text("Sword of Honour"), text("Moby Dick"), text("The Lord of the Rings")
    )
    JsonPath.query("$.store.book[?(@.price < 20 && @.price > 8.96)].title", goessnerJson) should findOrderedElements(
      text("Sword of Honour"), text("Moby Dick")
    )

  }

  "Recursive" should "honor filters directly on root" in {
    JsonPath.query("$[?(@.id==19434 && @.foo==1)].foo", parseJson(json)) should findOrderedElements(int(1))
  }

  it should "honor recursive filters from root" in {
    JsonPath.query("$..*[?(@.id==19434 && @.foo==1)].foo", parseJson(json)) should findOrderedElements(int(1))
  }

  "veggies" should "foo" in {
    JsonPath.query("""$..vegetable[?(@.color=='green')].name""", parseJson(veggies)) should findOrderedElements(text("peas"))
  }
}

trait JsonPathMatchers {

  class OrderedElementsMatcher(expected: Traversable[Any]) extends Matcher[Either[JPError, Iterator[Any]]] {
    override def apply(input: Either[JPError, Iterator[Any]]): MatchResult =
      input match {
        case Right(it) =>
          val seq = it.toVector
          MatchResult(
            seq == expected,
            s"$seq does not contains the same elements as $expected",
            s"$seq is equal to $expected but it shouldn't"
          )
        case Left(e) => MatchResult(
          false,
          s"parsing issue, $e",
          s"parsing issue, $e"
        )
      }
  }
  def findOrderedElements(expected: Any*) = new OrderedElementsMatcher(expected)

  class ElementsMatcher(expected: Traversable[Any]) extends Matcher[Either[JPError, Iterator[Any]]] {
    override def apply(input: Either[JPError, Iterator[Any]]): MatchResult =
      input match {
        case Right(it) =>
          val actualSeq = it.toVector
          val expectedSeq = expected.toVector

          val missing = expectedSeq.diff(actualSeq)
          val added = actualSeq.diff(expectedSeq)
          MatchResult(
            missing.isEmpty && added.isEmpty,
            s"$actualSeq is missing $missing and should not contains $added",
            s"$actualSeq is equal to $expectedSeq but it shouldn't"
          )
        case Left(e) => MatchResult(
          false,
          s"parsing issue, $e",
          s"parsing issue, $e"
        )
      }
  }
  def findElements(expected: Any*) = new ElementsMatcher(expected)
}
