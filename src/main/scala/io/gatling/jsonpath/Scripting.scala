package io.gatling.jsonpath

// Ordered operations
trait ComparisonOperation {

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

object EqOperation extends ComparisonOperation {
	override def compare[T <% Ordered[T]](lhs: T, rhs: T) = lhs == rhs
}

object LessOperation extends ComparisonOperation {
	override def compare[T <% Ordered[T]](lhs: T, rhs: T) = lhs < rhs
}

object GreaterOperation extends ComparisonOperation {
	override def compare[T <% Ordered[T]](lhs: T, rhs: T) = lhs > rhs
}

object LessOrEqOperation extends ComparisonOperation {
	override def compare[T <% Ordered[T]](lhs: T, rhs: T) = lhs <= rhs
}

object GreaterOrEqOperation extends ComparisonOperation {
	override def compare[T <% Ordered[T]](lhs: T, rhs: T) = lhs >= rhs
}

// Binary boolean operations
trait BinaryBooleanOperation {
	def apply(lhs: Boolean, rhs: Boolean): Boolean
}

object AndOperation extends BinaryBooleanOperation {
	def apply(lhs: Boolean, rhs: Boolean) = lhs && rhs
}

object OrOperation extends BinaryBooleanOperation {
	def apply(lhs: Boolean, rhs: Boolean) = lhs || rhs
}