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

sealed trait ComparisonOperator {
  def apply(lhs: JsonElement[_], rhs: JsonElement[_]): Boolean
}

// Comparison operators
sealed trait ComparisonWithOrderingOperator extends ComparisonOperator {

  protected def compare[T: Ordering](lhs: T, rhs: T): Boolean

  def apply(lhs: JsonElement[_], rhs: JsonElement[_]): Boolean =
    (lhs, rhs) match {
      case (StringValue(left), StringValue(right))   => compare(left, right)
      case (BooleanValue(left), BooleanValue(right)) => compare(left, right)
      case (LongValue(left), LongValue(right))       => compare(left, right)
      case (LongValue(left), DoubleValue(right))     => compare(left.doubleValue(), right)
      case (DoubleValue(left), LongValue(right))     => compare(left, right.doubleValue())
      case (DoubleValue(left), DoubleValue(right))   => compare(left, right)
      case _                                         => false
    }
}

case object EqWithOrderingOperator extends ComparisonWithOrderingOperator {
  protected def compare[T: Ordering](lhs: T, rhs: T): Boolean = Ordering[T].equiv(lhs, rhs)
}

case object EqOperator extends ComparisonOperator {
  override def apply(lhs: JsonElement[_], rhs: JsonElement[_]): Boolean =
    (lhs, rhs) match {
      case (NullValue, NullValue) => true
      case _                      => EqWithOrderingOperator(lhs, rhs)
    }
}

case object NotEqOperator extends ComparisonOperator {
  override def apply(lhs: JsonElement[_], rhs: JsonElement[_]): Boolean = !EqOperator(lhs, rhs)
}

case object LessOperator extends ComparisonWithOrderingOperator {
  override protected def compare[T: Ordering](lhs: T, rhs: T): Boolean = Ordering[T].lt(lhs, rhs)
}

case object GreaterOperator extends ComparisonWithOrderingOperator {
  override protected def compare[T: Ordering](lhs: T, rhs: T): Boolean = Ordering[T].gt(lhs, rhs)
}

case object LessOrEqOperator extends ComparisonWithOrderingOperator {
  override protected def compare[T: Ordering](lhs: T, rhs: T): Boolean = Ordering[T].lteq(lhs, rhs)
}

case object GreaterOrEqOperator extends ComparisonWithOrderingOperator {
  override protected def compare[T: Ordering](lhs: T, rhs: T): Boolean = Ordering[T].gteq(lhs, rhs)
}

// Binary boolean operators
sealed trait BinaryBooleanOperator {
  def apply(lhs: Boolean, rhs: Boolean): Boolean
}

case object AndOperator extends BinaryBooleanOperator {
  override def apply(lhs: Boolean, rhs: Boolean): Boolean = lhs && rhs
}

case object OrOperator extends BinaryBooleanOperator {
  override def apply(lhs: Boolean, rhs: Boolean): Boolean = lhs || rhs
}
