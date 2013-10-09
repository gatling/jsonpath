package io.gatling.jsonpath.benchmark

import org.scalameter.api._
import io.gatling.jsonpath.jsonsmart.JsonPath


object JsonPathBenchmark extends PerformanceTest.Quickbenchmark {

	val goessnerJson = """
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
"""

	val queries = Gen.enumeration("goessnerQueries")("$.store.book[*].author", "$..author", "$.store.*", "$.store..price",
		"$..book[2]", "$..book[-1:]", "$..book[:2]", "$..*", "$.store.book[*].niçôlàs['nico']['foo'][*].bar[1:-2:3]", "$.store['book'][:2].title")

	performance of "JsonPath" in {

		measure method "query resolving" in {
			using(queries) in {
				JsonPath.queryJsonString(_, goessnerJson).right.map(_.toVector)
			}
		}

	}
}