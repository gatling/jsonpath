package io.gatling.jsonpath

// Comparison operators
trait ComparisonOperator {

	def isNumber(a: Any) = isIntegralNumber(a) || isFloatingPointNumber(a)

	def isIntegralNumber(a: Any) = a.isInstanceOf[Int] || a.isInstanceOf[Long]
	def asIntegralNumber(a: Any): Option[Long] = a match {
		case a: Integer => Some(a.toLong)
		case a: Long => Some(a)
		case _ => None
	}

	def isFloatingPointNumber(a: Any) = a.isInstanceOf[Float] || a.isInstanceOf[Double]
	def asFloatingPointNumber(a: Any): Option[Double] = a match {
		case a: Int => Some(a.toDouble)
		case a: Long => Some(a.toDouble)
		case a: Float => Some(a.toDouble)
		case a: Double => Some(a)
		case _ => None
	}

	def compare[T <% Ordered[T]](lhs: T, rhs: T): Boolean

	def apply(lhs: Any, rhs: Any): Boolean =
		(lhs, rhs) match {
			case (s1: String, s2: String) => compare(s1, s2)
			case (i1, i2) if (isNumber(lhs) && isNumber(rhs)) =>
				if (isIntegralNumber(lhs) && isIntegralNumber(rhs))
					compare(asIntegralNumber(i1), asIntegralNumber(i2))
				else
					compare(asFloatingPointNumber(i1), asFloatingPointNumber(i2))
			case _ => false
		}
}

object EqOperator extends ComparisonOperator {
	override def compare[T <% Ordered[T]](lhs: T, rhs: T) = lhs == rhs
}

object LessOperator extends ComparisonOperator {
	override def compare[T <% Ordered[T]](lhs: T, rhs: T) = lhs < rhs
}

object GreaterOperator extends ComparisonOperator {
	override def compare[T <% Ordered[T]](lhs: T, rhs: T) = lhs > rhs
}

object LessOrEqOperator extends ComparisonOperator {
	override def compare[T <% Ordered[T]](lhs: T, rhs: T) = lhs <= rhs
}

object GreaterOrEqOperator extends ComparisonOperator {
	override def compare[T <% Ordered[T]](lhs: T, rhs: T) = lhs >= rhs
}

// Binary boolean operators
trait BinaryBooleanOperator {
	def apply(lhs: Boolean, rhs: Boolean): Boolean
}

object AndOperator extends BinaryBooleanOperator {
	def apply(lhs: Boolean, rhs: Boolean) = lhs && rhs
}

object OrOperator extends BinaryBooleanOperator {
	def apply(lhs: Boolean, rhs: Boolean) = lhs || rhs
}