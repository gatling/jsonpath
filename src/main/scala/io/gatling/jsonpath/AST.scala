package io.gatling.jsonpath

sealed trait AstToken
sealed trait PathToken extends AstToken

sealed trait FieldAccessor extends PathToken
case class Root() extends FieldAccessor
case class Field(val name: String, val recursive: Boolean = false) extends FieldAccessor
case class MultiField(val names :List[String]) extends FieldAccessor
case class AnyField(val recursive: Boolean = false) extends FieldAccessor


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

case class CurrentObject() extends PathToken
sealed trait FilterToken extends AstToken

sealed trait JPNumber extends FilterToken
case class JPLong(val i:Long) extends JPNumber
case class JPDouble(val d:Double) extends JPNumber 
case class JPString(val s:String) extends FilterToken

case class SubQuery(val path:List[PathToken]) extends FilterToken

case class HasFilter(val query:SubQuery) extends PathToken

/**
 * Currently supported operators: ==, <=, <, >=, >
 */
case class BinaryOpFilter(val operator:String, val lhs:FilterToken, val rhs:FilterToken) extends PathToken



// is it necessary ?
object AST {
	val rootObject = Root()
	val anyField = AnyField(false)
	val anyRecursiveField = AnyField(true)
	val currentObject = CurrentObject()
}
