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

import play.api.libs.json.{ JsBoolean, JsNull, JsNumber, JsString, JsValue }

sealed trait ComparisonOperator {
  def apply(lhs: JsValue, rhs: JsValue): Boolean
}

// Comparison operators
sealed trait ComparisonWithOrderingOperator extends ComparisonOperator {

  protected def compare[T: Ordering](lhs: T, rhs: T): Boolean

  def apply(lhs: JsValue, rhs: JsValue): Boolean =
    (lhs, rhs) match {
      case (JsString(left), JsString(right))   => compare(left, right)
      case (JsBoolean(left), JsBoolean(right)) => compare(left, right)
      case (JsNumber(left), JsNumber(right))   => compare(left, right)
      case _                                   => false
    }
}

case object EqWithOrderingOperator extends ComparisonWithOrderingOperator {
  protected def compare[T: Ordering](lhs: T, rhs: T): Boolean = Ordering[T].equiv(lhs, rhs)
}

case object EqOperator extends ComparisonOperator {
  override def apply(lhs: JsValue, rhs: JsValue): Boolean =
    (lhs, rhs) match {
      case (JsNull, JsNull) => true
      case _                => EqWithOrderingOperator(lhs, rhs)
    }
}

case object NotEqOperator extends ComparisonOperator {
  override def apply(lhs: JsValue, rhs: JsValue): Boolean = !EqOperator(lhs, rhs)
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
