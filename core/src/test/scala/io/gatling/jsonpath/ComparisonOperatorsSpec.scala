/*
 * Copyright 2011-2019 GatlingCorp (https://gatling.io)
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

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

class ComparisonOperatorsSpec
  extends FlatSpec
  with Matchers
  with GeneratorDrivenPropertyChecks {

  "comparison operators" should "return false if types aren't compatible" in {
    forAll(arbitrary[String], arbitrary[Int]) { (string, int) =>
      val lhn = StringValue(string)
      val rhn = LongValue(int)
      LessOperator(lhn, rhn) shouldBe false
      GreaterOperator(lhn, rhn) shouldBe false
      LessOrEqOperator(lhn, rhn) shouldBe false
      GreaterOrEqOperator(lhn, rhn) shouldBe false
    }

    forAll(arbitrary[Boolean], arbitrary[String]) { (bool, string) =>
      val lhn = BooleanValue(bool)
      val rhn = StringValue(string)
      LessOperator(lhn, rhn) shouldBe false
      GreaterOperator(lhn, rhn) shouldBe false
      LessOrEqOperator(lhn, rhn) shouldBe false
      GreaterOrEqOperator(lhn, rhn) shouldBe false
    }

    forAll(arbitrary[Int], arbitrary[String]) { (int, string) =>
      val lhn = LongValue(int)
      val rhn = StringValue(string)
      LessOperator(lhn, rhn) shouldBe false
      GreaterOperator(lhn, rhn) shouldBe false
      LessOrEqOperator(lhn, rhn) shouldBe false
      GreaterOrEqOperator(lhn, rhn) shouldBe false
    }
  }

  it should "properly compare Strings" in {
    forAll(arbitrary[String], arbitrary[String]) { (val1, val2) =>
      val lhn = StringValue(val1)
      val rhn = StringValue(val2)
      LessOperator(lhn, rhn) shouldBe (val1 < val2)
      GreaterOperator(lhn, rhn) shouldBe (val1 > val2)
      LessOrEqOperator(lhn, rhn) shouldBe (val1 <= val2)
      GreaterOrEqOperator(lhn, rhn) shouldBe (val1 >= val2)
    }
  }

  it should "properly compare Booleans" in {
    forAll(arbitrary[Boolean], arbitrary[Boolean]) { (val1, val2) =>
      val lhn = BooleanValue(val1)
      val rhn = BooleanValue(val2)
      LessOperator(lhn, rhn) shouldBe (val1 < val2)
      GreaterOperator(lhn, rhn) shouldBe (val1 > val2)
      LessOrEqOperator(lhn, rhn) shouldBe (val1 <= val2)
      GreaterOrEqOperator(lhn, rhn) shouldBe (val1 >= val2)
    }
  }

  it should "properly compare Int with other numeric types" in {
    forAll(arbitrary[Int], arbitrary[Int]) { (val1, val2) =>
      val lhn = LongValue(val1)
      val rhn = LongValue(val2)
      LessOperator(lhn, rhn) shouldBe (val1 < val2)
      GreaterOperator(lhn, rhn) shouldBe (val1 > val2)
      LessOrEqOperator(lhn, rhn) shouldBe (val1 <= val2)
      GreaterOrEqOperator(lhn, rhn) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Int], arbitrary[Long]) { (val1, val2) =>
      val lhn = LongValue(val1)
      val rhn = LongValue(val2)
      LessOperator(lhn, rhn) shouldBe (val1 < val2)
      GreaterOperator(lhn, rhn) shouldBe (val1 > val2)
      LessOrEqOperator(lhn, rhn) shouldBe (val1 <= val2)
      GreaterOrEqOperator(lhn, rhn) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Int], arbitrary[Double]) { (val1, val2) =>
      val lhn = LongValue(val1)
      val rhn = DoubleValue(val2)
      LessOperator(lhn, rhn) shouldBe (val1 < val2)
      GreaterOperator(lhn, rhn) shouldBe (val1 > val2)
      LessOrEqOperator(lhn, rhn) shouldBe (val1 <= val2)
      GreaterOrEqOperator(lhn, rhn) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Int], arbitrary[Float]) { (val1, val2) =>
      val lhn = LongValue(val1)
      val rhn = DoubleValue(val2)
      LessOperator(lhn, rhn) shouldBe (val1 < val2)
      GreaterOperator(lhn, rhn) shouldBe (val1 > val2)
      LessOrEqOperator(lhn, rhn) shouldBe (val1 <= val2)
      GreaterOrEqOperator(lhn, rhn) shouldBe (val1 >= val2)
    }
  }

  it should "properly compare Long with other numeric types" in {
    forAll(arbitrary[Long], arbitrary[Int]) { (val1, val2) =>
      val lhn = LongValue(val1)
      val rhn = LongValue(val2)
      LessOperator(lhn, rhn) shouldBe (val1 < val2)
      GreaterOperator(lhn, rhn) shouldBe (val1 > val2)
      LessOrEqOperator(lhn, rhn) shouldBe (val1 <= val2)
      GreaterOrEqOperator(lhn, rhn) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Long], arbitrary[Long]) { (val1, val2) =>
      val lhn = LongValue(val1)
      val rhn = LongValue(val2)
      LessOperator(lhn, rhn) shouldBe (val1 < val2)
      GreaterOperator(lhn, rhn) shouldBe (val1 > val2)
      LessOrEqOperator(lhn, rhn) shouldBe (val1 <= val2)
      GreaterOrEqOperator(lhn, rhn) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Long], arbitrary[Double]) { (val1, val2) =>
      val lhn = LongValue(val1)
      val rhn = DoubleValue(val2)
      LessOperator(lhn, rhn) shouldBe (val1 < val2)
      GreaterOperator(lhn, rhn) shouldBe (val1 > val2)
      LessOrEqOperator(lhn, rhn) shouldBe (val1 <= val2)
      GreaterOrEqOperator(lhn, rhn) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Long], arbitrary[Float]) { (val1, val2) =>
      val lhn = LongValue(val1)
      val rhn = DoubleValue(val2)
      LessOperator(lhn, rhn) shouldBe (val1 < val2)
      GreaterOperator(lhn, rhn) shouldBe (val1 > val2)
      LessOrEqOperator(lhn, rhn) shouldBe (val1 <= val2)
      GreaterOrEqOperator(lhn, rhn) shouldBe (val1 >= val2)
    }
  }

  it should "properly compare Double with other numeric types" in {
    forAll(arbitrary[Double], arbitrary[Int]) { (val1, val2) =>
      val lhn = DoubleValue(val1)
      val rhn = LongValue(val2)
      LessOperator(lhn, rhn) shouldBe (val1 < val2)
      GreaterOperator(lhn, rhn) shouldBe (val1 > val2)
      LessOrEqOperator(lhn, rhn) shouldBe (val1 <= val2)
      GreaterOrEqOperator(lhn, rhn) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Double], arbitrary[Long]) { (val1, val2) =>
      val lhn = DoubleValue(val1)
      val rhn = LongValue(val2)
      LessOperator(lhn, rhn) shouldBe (val1 < val2)
      GreaterOperator(lhn, rhn) shouldBe (val1 > val2)
      LessOrEqOperator(lhn, rhn) shouldBe (val1 <= val2)
      GreaterOrEqOperator(lhn, rhn) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Double], arbitrary[Double]) { (val1, val2) =>
      val lhn = DoubleValue(val1)
      val rhn = DoubleValue(val2)
      LessOperator(lhn, rhn) shouldBe (val1 < val2)
      GreaterOperator(lhn, rhn) shouldBe (val1 > val2)
      LessOrEqOperator(lhn, rhn) shouldBe (val1 <= val2)
      GreaterOrEqOperator(lhn, rhn) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Double], arbitrary[Float]) { (val1, val2) =>
      val lhn = DoubleValue(val1)
      val rhn = DoubleValue(val2)
      LessOperator(lhn, rhn) shouldBe (val1 < val2)
      GreaterOperator(lhn, rhn) shouldBe (val1 > val2)
      LessOrEqOperator(lhn, rhn) shouldBe (val1 <= val2)
      GreaterOrEqOperator(lhn, rhn) shouldBe (val1 >= val2)
    }
  }

  it should "properly compare Float with other numeric types" in {
    forAll(arbitrary[Float], arbitrary[Int]) { (val1, val2) =>
      val lhn = DoubleValue(val1)
      val rhn = LongValue(val2)
      LessOperator(lhn, rhn) shouldBe (val1 < val2)
      GreaterOperator(lhn, rhn) shouldBe (val1 > val2)
      LessOrEqOperator(lhn, rhn) shouldBe (val1 <= val2)
      GreaterOrEqOperator(lhn, rhn) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Float], arbitrary[Long]) { (val1, val2) =>
      val lhn = DoubleValue(val1)
      val rhn = LongValue(val2)
      LessOperator(lhn, rhn) shouldBe (val1 < val2)
      GreaterOperator(lhn, rhn) shouldBe (val1 > val2)
      LessOrEqOperator(lhn, rhn) shouldBe (val1 <= val2)
      GreaterOrEqOperator(lhn, rhn) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Float], arbitrary[Double]) { (val1, val2) =>
      val lhn = DoubleValue(val1)
      val rhn = DoubleValue(val2)
      LessOperator(lhn, rhn) shouldBe (val1 < val2)
      GreaterOperator(lhn, rhn) shouldBe (val1 > val2)
      LessOrEqOperator(lhn, rhn) shouldBe (val1 <= val2)
      GreaterOrEqOperator(lhn, rhn) shouldBe (val1 >= val2)
    }

    forAll(arbitrary[Float], arbitrary[Float]) { (val1, val2) =>
      val lhn = DoubleValue(val1)
      val rhn = DoubleValue(val2)
      LessOperator(lhn, rhn) shouldBe (val1 < val2)
      GreaterOperator(lhn, rhn) shouldBe (val1 > val2)
      LessOrEqOperator(lhn, rhn) shouldBe (val1 <= val2)
      GreaterOrEqOperator(lhn, rhn) shouldBe (val1 >= val2)
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
