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

import io.gatling.jsonpath.AST._

import scala.math.abs

case class JPError(reason: String)

abstract class BaseJsonPath[T] {
  private val JsonPathParser = ThreadLocal.withInitial[Parser](() => new Parser)

  def compile(query: String): Either[JPError, JsonPathQuery] =
    JsonPathParser.get.compile(query) match {
      case Parser.Success(q, _) => Right(new JsonPathQuery(q))
      case ns: Parser.NoSuccess => Left(JPError(ns.msg))
    }

  protected def mapJsonObject(jsonObject: T): JsonElement[_]

  def query(query: String, jsonObject: T): Either[JPError, Iterator[Any]] =
    compile(query).right.map(_.query(mapJsonObject(jsonObject)))
}

class JsonPathQuery(path: List[PathToken]) extends Serializable {
  private def getValue(element: JsonElement[_]): Any = element match {
    case obj: ObjectElement  => getObject(obj)
    case array: ArrayElement => getArray(array)
    case LongValue(value)    => value
    case DoubleValue(value)  => value
    case BooleanValue(value) => value
    case StringValue(value)  => value
    case NullValue           => null
  }

  private def getObject(obj: ObjectElement): Map[String, Any] =
    obj.values.map { case (key, value) => key -> getValue(value) }.toMap

  private def getArray(array: ArrayElement): Seq[Any] =
    array.values.map(getValue).toList

  def query(jsonNode: JsonElement[_]): Iterator[Any] =
    new JsonPathWalker(jsonNode, path).walk().map(getValue)
}

class JsonPathWalker(rootNode: JsonElement[_], fullPath: List[PathToken]) {

  def walk(): Iterator[JsonElement[_]] = walk(rootNode, fullPath)

  private[this] def walk(node: JsonElement[_], path: List[PathToken]): Iterator[JsonElement[_]] =
    path match {
      case head :: tail => walk1(node, head).flatMap(walk(_, tail))
      case _            => Iterator.single(node)
    }

  private[this] def walk1(node: JsonElement[_], query: PathToken): Iterator[JsonElement[_]] =
    query match {
      case RootNode    => Iterator.single(rootNode)

      case CurrentNode => Iterator.single(node)

      case Field(name) => node match {
        case obj: ObjectElement if obj.contains(name) =>
          Iterator.single(obj(name))
        case _ => Iterator.empty
      }

      case RecursiveField(name) => new RecursiveFieldIterator(node, name)

      case MultiField(fieldNames) => node match {
        case obj: ObjectElement =>
          // don't use collect on iterator with filter causes (executed twice)
          fieldNames.iterator.filter(obj.contains).map(obj(_))
        case _ => Iterator.empty
      }

      case AnyField => node match {
        case obj: ObjectElement => obj.values.map(_._2)
        case _                  => Iterator.empty
      }

      case ArraySlice(None, None, 1) => node match {
        case array: ArrayElement => array.values
        case _                   => Iterator.empty
      }

      case ArraySlice(start, stop, step) => node match {
        case array: ArrayElement => sliceArray(array, start, stop, step)
        case _                   => Iterator.empty
      }

      case ArrayRandomAccess(indices) => node match {
        case array: ArrayElement => indices.iterator.collect {
          case i if i >= 0 && i < array.size  => array(i)
          case i if i < 0 && i >= -array.size => array(i + array.size)
        }
        case _ => Iterator.empty
      }

      case RecursiveFilterToken(filterToken) => new RecursiveDataIterator(node).flatMap(applyFilter(_, filterToken))

      case filterToken: FilterToken          => applyFilter(node, filterToken)

      case RecursiveAnyField                 => new RecursiveNodeIterator(node)
    }

  private[this] def applyFilter(currentNode: JsonElement[_], filterToken: FilterToken): Iterator[JsonElement[_]] = {

    def resolveSubQuery(node: JsonElement[_], q: List[AST.PathToken], nextOp: JsonElement[_] => Boolean): Boolean = {
      val it = walk(node, q)
      it.hasNext && nextOp(it.next())
    }

    def applyBinaryOpWithResolvedLeft(node: JsonElement[_], op: ComparisonOperator, lhsNode: JsonElement[_], rhs: FilterValue): Boolean =
      rhs match {
        case FilterDirectValue(valueNode) => op(lhsNode, valueNode)
        case SubQuery(q)                  => resolveSubQuery(node, q, op(lhsNode, _))
      }

    def applyBinaryOp(op: ComparisonOperator, lhs: FilterValue, rhs: FilterValue): JsonElement[_] => Boolean =
      lhs match {
        case FilterDirectValue(valueNode) => applyBinaryOpWithResolvedLeft(_, op, valueNode, rhs)
        case SubQuery(q)                  => node => resolveSubQuery(node, q, applyBinaryOpWithResolvedLeft(node, op, _, rhs))
      }

    def elementsToFilter(node: JsonElement[_]): Iterator[JsonElement[_]] =
      node match {
        case array: ArrayElement => array.values
        case obj: ObjectElement  => Iterator.single(obj)
        case _                   => Iterator.empty
      }

    def evaluateFilter(filterToken: FilterToken): JsonElement[_] => Boolean =
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

  private[this] def sliceArray(array: ArrayElement, start: Option[Int], stop: Option[Int], step: Int): Iterator[JsonElement[_]] = {
    val size = array.size

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

    val elements: Iterator[JsonElement[_]] = if (step < 0) Iterator.range(array.size - 1, -1, -1).map(array(_)) else array.values
    val fromStartToEnd = elements.slice(absStart, absEnd)

    if (absStep == 1)
      fromStartToEnd
    else
      fromStartToEnd.grouped(absStep).map(_.head)
  }
}
