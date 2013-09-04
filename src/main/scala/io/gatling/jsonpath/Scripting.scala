package io.gatling.jsonpath

trait OrderedOperation {
	def apply[T <% Ordered[T]](lhs: T, rhs: T): Boolean
}

object EqOperation extends OrderedOperation {
	def apply[T <% Ordered[T]](lhs: T, rhs: T) = lhs == rhs
}

object LessOperation extends OrderedOperation {
	def apply[T <% Ordered[T]](lhs: T, rhs: T) = lhs < rhs
}

object GreaterOperation extends OrderedOperation {
	def apply[T <% Ordered[T]](lhs: T, rhs: T) = lhs > rhs
}

object LessOrEqOperation extends OrderedOperation {
	def apply[T <% Ordered[T]](lhs: T, rhs: T) = lhs <= rhs
}

object GreaterOrEqOperation extends OrderedOperation {
	def apply[T <% Ordered[T]](lhs: T, rhs: T) = lhs >= rhs
}
