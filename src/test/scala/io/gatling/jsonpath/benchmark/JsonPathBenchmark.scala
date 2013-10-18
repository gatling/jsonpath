package io.gatling.jsonpath.benchmark

import org.scalameter.api._
import io.gatling.jsonpath.jsonsmart.JsonPath
import net.minidev.json.JSONValue

object JsonPathBenchmark extends PerformanceTest.Quickbenchmark {

	val goessnerJson = JSONValue.parse("""
		{ "store": {
		    "book": [ 
		      { "category": "reference",
		        "author": "Nigel Rees",
		        "title": "Sayings of the Century",
		        "price": 8.95
		      },
		      { "category": "fiction",
		        "author": "Evelyn Waugh",
		        "title": "Sword of Honour",
		        "price": 12.99
		      },
		      { "category": "fiction",
		        "author": "Herman Melville",
		        "title": "Moby Dick",
		        "isbn": "0-553-21311-3",
		        "price": 8.99
		      },
		      { "category": "fiction",
		        "author": "J. R. R. Tolkien",
		        "title": "The Lord of the Rings",
		        "isbn": "0-395-19395-8",
		        "price": 22.99
		      }
		    ],
		    "bicycle": {
		      "color": "red",
		      "price": 19.95
		    }
		  }
		}
""")

	val queries = Gen.enumeration("goessnerQueries")("$.store.book[*].author",
		"$..author",
		"$.store.*",
		"$.store..price",
		"$..book[2].title",
		"$..book[-1:].title",
		"$..book[:2].title",
		"$..*",
		"$.store.book[*].niçôlàs['nico']['foo'][*].bar[1:-2:3]",
		"$.store['book'][:2].title",
		"$.store.book[?(@.isbn)].title",
		"$.store.book[?(@.category == 'fiction')].title",
		"$.store.book[?(@.price < 10 && @.price >4)].title")

	val compiledQueries = queries.map(JsonPath.compile(_))

	performance of "JsonPath" in {

		measure method "query resolving" in {
			using(queries) in {
				JsonPath.queryJsonObject(_, goessnerJson).right.map(_.toVector)
			}
		}

		measure method "pre-compiled query resolving" in {
			using(compiledQueries) in {
				q => q.right.map(_.queryJsonObject(goessnerJson)).right.map(_.toVector)
			}
		}

	}
}