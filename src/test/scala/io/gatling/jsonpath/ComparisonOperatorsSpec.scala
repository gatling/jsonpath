/*
 * Copyright 2011-2018 GatlingCorp (http://gatling.io)
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

import org.scalacheck.{ Gen, Arbitrary }
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{ Matchers, FlatSpec }

class ComparisonOperatorsSpec
  extends FlatSpec
  with Matchers
  with GeneratorDrivenPropertyChecks {

  "comparison operators" should "return false if types aren't compatible" in {
    forAll(arbitrary[String], arbitrary[AnyVal]) { (s, anyVal) =>
      LessOperator(s, anyVal) shouldBe false
      GreaterOperator(s, anyVal) shouldBe false
      LessOrEqOperator(s, anyVal) shouldBe false
      GreaterOrEqOperator(s, anyVal) shouldBe false
    }

    forAll(arbitrary[Boolean], arbitrary[String]) { (s, anyVal) =>
      LessOperator(s, anyVal) shouldBe false
      GreaterOperator(s, anyVal) shouldBe false
      LessOrEqOperator(s, anyVal) shouldBe false
      GreaterOrEqOperator(s, anyVal) shouldBe false
    }

    forAll(arbitrary[AnyVal], arbitrary[String]) { (s, anyVal) =>
      LessOperator(s, anyVal) shouldBe false
      GreaterOperator(s, anyVal) shouldBe false
      LessOrEqOperator(s, anyVal) shouldBe false
      GreaterOrEqOperator(s, anyVal) shouldBe false
    }
  }

  it should "properly compare Strings" in {
    forAll(arbitrary[String], arbitrary[String]) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }
  }

  it should "properly compare Booleans" in {
    forAll(arbitrary[Boolean], arbitrary[Boolean]) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }
  }

  it should "properly compare Int with other numeric types" in {
    forAll(arbitrary[Int], arbitrary[Int]) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Int], arbitrary[Long]) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Int], arbitrary[Double]) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Int], arbitrary[Float]) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }
  }

  it should "properly compare Long with other numeric types" in {
    forAll(arbitrary[Long], arbitrary[Int]) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Long], arbitrary[Long]) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Long], arbitrary[Double]) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Long], arbitrary[Float]) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }
  }

  it should "properly compare Double with other numeric types" in {
    forAll(arbitrary[Double], arbitrary[Int]) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Double], arbitrary[Long]) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Double], arbitrary[Double]) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Double], arbitrary[Float]) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }
  }

  it should "properly compare Float with other numeric types" in {
    forAll(arbitrary[Float], arbitrary[Int]) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Float], arbitrary[Long]) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Float], arbitrary[Double]) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Float], arbitrary[Float]) { (val1, val2) =>
      LessOperator(val1, val2) shouldBe (val1 < val2)
      GreaterOperator(val1, val2) shouldBe (val1 > val2)
      LessOrEqOperator(val1, val2) shouldBe (val1 <= val2)
      GreaterOrEqOperator(val1, val2) shouldBe (val1 >= val2)
    }
  }

  "AndOperator" should "&& the lhs and rhs" in {
    forAll(Arbitrary.arbBool.arbitrary, Arbitrary.arbBool.arbitrary) { (b1, b2) =>
      AndOperator(b1, b2) shouldBe (b1 && b2)
    }
  }

  "OrOperator" should "|| the lhs and rhs" in {
    forAll(Arbitrary.arbBool.arbitrary, Arbitrary.arbBool.arbitrary) { (b1, b2) =>
      OrOperator(b1, b2) shouldBe (b1 || b2)
    }
  }
}
