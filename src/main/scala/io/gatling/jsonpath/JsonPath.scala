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

import scala.math.abs
import io.gatling.jsonpath.AST._
import play.api.libs.json.{ JsArray, JsObject, JsValue }

case class JPError(reason: String)

object JsonPath {
  private val JsonPathParser = ThreadLocal.withInitial[Parser](() => new Parser)

  def compile(query: String): Either[JPError, JsonPathQuery] =
    JsonPathParser.get.compile(query) match {
      case Parser.Success(q, _) => Right(new JsonPathQuery(q))
      case ns: Parser.NoSuccess => Left(JPError(ns.msg))
    }

  def query(query: String, jsonObject: JsValue): Either[JPError, Iterator[JsValue]] =
    compile(query).right.map(_.query(jsonObject))
}

class JsonPathQuery(path: List[PathToken]) extends Serializable {
  def query(jsonNode: JsValue): Iterator[JsValue] = new JsonPathWalker(jsonNode, path).walk()
}

class JsonPathWalker(rootNode: JsValue, fullPath: List[PathToken]) {

  def walk(): Iterator[JsValue] = walk(rootNode, fullPath)

  private[this] def walk(node: JsValue, path: List[PathToken]): Iterator[JsValue] =
    path match {
      case head :: tail => walk1(node, head).flatMap(walk(_, tail))
      case _            => Iterator.single(node)
    }

  private[this] def walk1(node: JsValue, query: PathToken): Iterator[JsValue] =
    query match {
      case RootNode    => Iterator.single(rootNode)

      case CurrentNode => Iterator.single(node)

      case Field(name) => node match {
        case obj: JsObject if obj.keys.contains(name) =>
          Iterator.single(obj.value(name))
        case _ => Iterator.empty
      }

      case RecursiveField(name) => new RecursiveFieldIterator(node, name)

      case MultiField(fieldNames) => node match {
        case obj: JsObject =>
          // don't use collect on iterator with filter causes (executed twice)
          fieldNames.iterator.filter(obj.keys.contains).map(obj.value)
        case _ => Iterator.empty
      }

      case AnyField => node match {
        case obj: JsObject => obj.values.iterator
        case _             => Iterator.empty
      }

      case ArraySlice(None, None, 1) => node match {
        case array: JsArray => array.value.iterator
        case _              => Iterator.empty
      }

      case ArraySlice(start, stop, step) => node match {
        case array: JsArray => sliceArray(array, start, stop, step)
        case _              => Iterator.empty
      }

      case ArrayRandomAccess(indices) => node match {
        case array: JsArray => indices.iterator.collect {
          case i if i >= 0 && i < array.value.size  => array(i)
          case i if i < 0 && i >= -array.value.size => array(i + array.value.size)
        }
        case _ => Iterator.empty
      }

      case RecursiveFilterToken(filterToken) => new RecursiveDataIterator(node).flatMap(applyFilter(_, filterToken))

      case filterToken: FilterToken          => applyFilter(node, filterToken)

      case RecursiveAnyField                 => new RecursiveNodeIterator(node)
    }

  private[this] def applyFilter(currentNode: JsValue, filterToken: FilterToken): Iterator[JsValue] = {

    def resolveSubQuery(node: JsValue, q: List[AST.PathToken], nextOp: JsValue => Boolean): Boolean = {
      val it = walk(node, q)
      it.hasNext && nextOp(it.next())
    }

    def applyBinaryOpWithResolvedLeft(node: JsValue, op: ComparisonOperator, lhsNode: JsValue, rhs: FilterValue): Boolean =
      rhs match {
        case FilterDirectValue(valueNode) => op(lhsNode, valueNode)
        case SubQuery(q)                  => resolveSubQuery(node, q, op(lhsNode, _))
      }

    def applyBinaryOp(op: ComparisonOperator, lhs: FilterValue, rhs: FilterValue): JsValue => Boolean =
      lhs match {
        case FilterDirectValue(valueNode) => applyBinaryOpWithResolvedLeft(_, op, valueNode, rhs)
        case SubQuery(q)                  => node => resolveSubQuery(node, q, applyBinaryOpWithResolvedLeft(node, op, _, rhs))
      }

    def elementsToFilter(node: JsValue): Iterator[JsValue] =
      node match {
        case array: JsArray => array.value.iterator
        case obj: JsObject  => Iterator.single(obj)
        case _              => Iterator.empty
      }

    def evaluateFilter(filterToken: FilterToken): JsValue => Boolean =
      filterToken match {
        case HasFilter(subQuery) =>
          walk(_, subQuery.path).hasNext

        case ComparisonFilter(op, lhs, rhs) =>
          applyBinaryOp(op, lhs, rhs)

        case BooleanFilter(op, filter1, filter2) =>
          val f1 = evaluateFilter(filter1)
          val f2 = evaluateFilter(filter2)
          node => op(f1(node), f2(node))
      }

    val filterFunction = evaluateFilter(filterToken)
    elementsToFilter(currentNode).filter(filterFunction)
  }

  private[this] def sliceArray(array: JsArray, start: Option[Int], stop: Option[Int], step: Int): Iterator[JsValue] = {
    val size = array.value.size

    def lenRelative(x: Int) = if (x >= 0) x else size + x

    def stepRelative(x: Int) = if (step >= 0) x else -1 - x

    def relative(x: Int) = lenRelative(stepRelative(x))

    val absStart = start match {
      case Some(v) => relative(v)
      case _       => 0
    }
    val absEnd = stop match {
      case Some(v) => relative(v)
      case _       => size
    }
    val absStep = abs(step)

    val elements: Iterator[JsValue] = if (step < 0) Iterator.range(array.value.size - 1, -1, -1).map(array.value) else array.value.iterator
    val fromStartToEnd = elements.slice(absStart, absEnd)

    if (absStep == 1)
      fromStartToEnd
    else
      fromStartToEnd.grouped(absStep).map(_.head)
  }
}
