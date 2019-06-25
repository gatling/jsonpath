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

object AST {
  sealed trait AstToken
  sealed trait PathToken extends AstToken

  sealed trait FieldAccessor extends PathToken
  case object RootNode extends FieldAccessor
  case class Field(name: String) extends FieldAccessor
  case class RecursiveField(name: String) extends FieldAccessor
  case class MultiField(names: List[String]) extends FieldAccessor
  case object AnyField extends FieldAccessor
  case object RecursiveAnyField extends FieldAccessor

  sealed trait ArrayAccessor extends PathToken

  /**
   * Slicing of an array, indices start at zero
   *
   * @param start is the first item that you want (of course)
   * @param stop  is the first item that you do not want
   * @param step  being positive or negative, defines whether you are moving
   */
  case class ArraySlice(start: Option[Int], stop: Option[Int], step: Int = 1) extends ArrayAccessor

  object ArraySlice {
    val All = ArraySlice(None, None)
  }

  case class ArrayRandomAccess(indices: List[Int]) extends ArrayAccessor

  // JsonPath Filter AST //////////////////////////////////////////////

  case object CurrentNode extends PathToken

  sealed trait FilterValue extends AstToken

  object FilterDirectValue {
    def long(value: Long) = FilterDirectValue(JsNumber(BigDecimal(value)))

    def double(value: Double) = FilterDirectValue(JsNumber(BigDecimal(value)))

    val True = FilterDirectValue(JsBoolean(true))
    val False = FilterDirectValue(JsBoolean(false))

    def string(value: String) = FilterDirectValue(JsString(value))

    val Null = FilterDirectValue(JsNull)
  }

  case class FilterDirectValue(node: JsValue) extends FilterValue

  case class SubQuery(path: List[PathToken]) extends FilterValue

  sealed trait FilterToken extends PathToken
  case class HasFilter(query: SubQuery) extends FilterToken
  case class ComparisonFilter(operator: ComparisonOperator, lhs: FilterValue, rhs: FilterValue) extends FilterToken
  case class BooleanFilter(operator: BinaryBooleanOperator, lhs: FilterToken, rhs: FilterToken) extends FilterToken

  case class RecursiveFilterToken(filter: FilterToken) extends PathToken

}
