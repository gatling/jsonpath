package io.gatling.jsonpath
import com.fasterxml.jackson.databind.node.ValueNode

// Comparison operators
sealed trait ComparisonOperator {

	def compare[T: Ordering](lhs: T, rhs: T): Boolean

	def apply(lhs: ValueNode, rhs: ValueNode): Boolean = lhs match {
		case s1 if s1.isTextual => rhs match {
			case s2 if s2.isTextual => compare(s1.textValue, s2.textValue)
			case _ => false
		}
		case f1 if f1.isFloat => rhs match {
			case x2 if x2.isFloat => compare(f1.floatValue, x2.floatValue)
			case x2 if x2.isInt => compare(f1.floatValue, x2.intValue)
			case x2 if x2.isDouble => compare(f1.floatValue, x2.doubleValue)
			case x2 if x2.isLong => compare(f1.floatValue, x2.longValue)
			case _ => false
		}
		case i1 if i1.isInt => rhs match {
			case x2 if x2.isInt || x2.isFloat => compare(i1.intValue, x2.intValue)
			case x2 if x2.isLong => compare(i1.longValue, x2.longValue)
			case x2 if x2.isDouble => compare(i1.doubleValue, x2.doubleValue)
			case _ => false
		}
		case d1 if d1.isDouble => rhs match {
			case x2 if x2.isDouble || x2.isInt || x2.isFloat => compare(d1.doubleValue, x2.doubleValue)
			case x2 if x2.isLong => compare(d1.longValue, x2.longValue)
			case _ => false
		}
		case l1 if l1.isLong => rhs match {
			case x2 if x2.isNumber => compare(l1.longValue, x2.longValue)
			case _ => false
		}
		case _ => false
	}
}

object EqOperator extends ComparisonOperator {
	override def compare[T: Ordering](lhs: T, rhs: T) = lhs == rhs
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
