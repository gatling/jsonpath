package io.gatling.jsonpath

sealed trait JsonElement[T]

abstract class ObjectElement extends JsonElement[ObjectElement] {
  def contains(key: String): Boolean
  def apply(key: String): JsonElement[_]
  def values: Iterator[(String, JsonElement[_])]
  def size: Int
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty
}

abstract class ArrayElement extends JsonElement[ArrayElement] {
  def values: Iterator[JsonElement[_]]
  def apply(index: Int): JsonElement[_]
  def size: Int
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty
}

abstract class ValueElement[T <: ValueElement[T]] extends JsonElement[T] with Comparable[T]

case class LongValue(value: Long) extends ValueElement[LongValue] {
  override def compareTo(o: LongValue): Int = value.compareTo(o.value)
}

case class DoubleValue(value: Double) extends ValueElement[DoubleValue] {
  override def compareTo(o: DoubleValue): Int = value.compareTo(o.value)
}

case class BooleanValue(value: Boolean) extends ValueElement[BooleanValue] {
  override def compareTo(o: BooleanValue): Int = value.compareTo(o.value)
}

case class StringValue(value: String) extends ValueElement[StringValue] {
  override def compareTo(o: StringValue): Int = value.compareTo(o.value)
}

object NullValue extends ValueElement[Nothing] {
  override def compareTo(o: Nothing): Int = 0
}
