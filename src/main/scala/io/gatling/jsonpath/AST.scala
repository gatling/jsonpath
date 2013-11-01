package io.gatling.jsonpath

object AST {
	sealed trait AstToken
	sealed trait PathToken extends AstToken

	sealed trait FieldAccessor extends PathToken
	case object RootNode extends FieldAccessor
	case class Field(val name: String, val recursive: Boolean = false) extends FieldAccessor
	case class MultiField(val names: List[String]) extends FieldAccessor
	case object AnyField extends FieldAccessor
	case object RecursiveAnyField extends FieldAccessor

	sealed trait ArrayAccessor extends PathToken

	/**
	 * Slicing of an array, indices start at zero
	 *
	 * @param start is the first item that you want (of course)
	 * @param stop is the first item that you do not want
	 * @param step, being positive or negative, defines whether you are moving
	 */
	case class ArraySlice(val start: Option[Int], val stop: Option[Int], val step: Int = 1) extends ArrayAccessor
	case class ArrayRandomAccess(val indices: List[Int]) extends ArrayAccessor

	// JsonPath Filter AST //////////////////////////////////////////////

	case object CurrentNode extends PathToken
	sealed trait FilterValue extends AstToken

	sealed trait JPNumber extends FilterValue
	case class JPLong(i: Long) extends JPNumber
	case class JPDouble(d: Double) extends JPNumber
	case class JPString(s: String) extends FilterValue

	case class SubQuery(path: List[PathToken]) extends FilterValue

	sealed trait FilterToken extends PathToken
	case class HasFilter(query: SubQuery) extends FilterToken
	case class ComparisonFilter(operator: ComparisonOperator, lhs: FilterValue, rhs: FilterValue) extends FilterToken
	case class BooleanFilter(fun: BinaryBooleanOperator, lhs: FilterToken, rhs: FilterToken) extends FilterToken
}
