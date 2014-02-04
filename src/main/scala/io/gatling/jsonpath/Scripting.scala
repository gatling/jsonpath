/**
 * Copyright 2011-2013 Gatling (gatling.io)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * 		http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.gatling.jsonpath

// Comparison operators
sealed trait ComparisonOperator {

	def compare[T: Ordering](lhs: T, rhs: T): Boolean

	def apply(lhs: Any, rhs: Any): Boolean = lhs match {
		case s1: String => rhs match {
			case s2: String => compare(s1, s2)
			case _ => false
		}
		case b1: Boolean => rhs match {
			case b2: Boolean => compare(b1, b2)
			case _ => false
		}
		case i1: Int => rhs match {
			case i2: Int => compare(i1, i2)
			case i2: Long => compare(i1, i2)
			case i2: Double => compare(i1, i2)
			case i2: Float => compare(i1, i2)
			case _ => false
		}
		case i1: Long => rhs match {
			case i2: Int => compare(i1, i2)
			case i2: Long => compare(i1, i2)
			case i2: Double => compare(i1, i2)
			case i2: Float => compare(i1, i2)
			case _ => false
		}
		case i1: Double => rhs match {
			case i2: Int => compare(i1, i2)
			case i2: Long => compare(i1, i2)
			case i2: Double => compare(i1, i2)
			case i2: Float => compare(i1, i2)
			case _ => false
		}
		case i1: Float => rhs match {
			case i2: Int => compare(i1, i2)
			case i2: Long => compare(i1, i2)
			case i2: Double => compare(i1, i2)
			case i2: Float => compare(i1, i2)
			case _ => false
		}
		case _ => false
	}
}

object EqOperator extends ComparisonOperator {
	override def compare[T: Ordering](lhs: T, rhs: T) = lhs == rhs
}

object NotEqOperator extends ComparisonOperator {
	override def compare[T: Ordering](lhs: T, rhs: T) = lhs != rhs
}

object LessOperator extends ComparisonOperator {
	override def compare[T: Ordering](lhs: T, rhs: T) = implicitly[Ordering[T]].lt(lhs, rhs)
}

object GreaterOperator extends ComparisonOperator {
	override def compare[T: Ordering](lhs: T, rhs: T) = implicitly[Ordering[T]].gt(lhs, rhs)
}

object LessOrEqOperator extends ComparisonOperator {
	override def compare[T: Ordering](lhs: T, rhs: T) = implicitly[Ordering[T]].lteq(lhs, rhs)
}

object GreaterOrEqOperator extends ComparisonOperator {
	override def compare[T: Ordering](lhs: T, rhs: T) = implicitly[Ordering[T]].gteq(lhs, rhs)
}

// Binary boolean operators
sealed trait BinaryBooleanOperator {
	def apply(lhs: Boolean, rhs: Boolean): Boolean
}

object AndOperator extends BinaryBooleanOperator {
	def apply(lhs: Boolean, rhs: Boolean) = lhs && rhs
}

object OrOperator extends BinaryBooleanOperator {
	def apply(lhs: Boolean, rhs: Boolean) = lhs || rhs
}
