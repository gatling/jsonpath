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

import java.util.function.Supplier

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.JsonNodeType._
import io.gatling.jsonpath.AST._

import scala.collection.JavaConverters._
import scala.math.abs

case class JPError(reason: String)

object JsonPath {
  private val JsonPathParser = ThreadLocal.withInitial[Parser](new Supplier[Parser]() {
    override def get(): Parser = new Parser
  })

  def compile(query: String): Either[JPError, JsonPath] =
    JsonPathParser.get.compile(query) match {
      case Parser.Success(q, _) => Right(new JsonPath(q))
      case ns: Parser.NoSuccess => Left(JPError(ns.msg))
    }

  def query(query: String, jsonObject: JsonNode): Either[JPError, Iterator[JsonNode]] =
    compile(query).right.map(_.query(jsonObject))
}

class JsonPath(path: List[PathToken]) {
  def query(jsonNode: JsonNode): Iterator[JsonNode] = new JsonPathWalker(jsonNode, path).walk()
}

class JsonPathWalker(rootNode: JsonNode, fullPath: List[PathToken]) {

  def walk(): Iterator[JsonNode] = walk(rootNode, fullPath)

  private[this] def walk(node: JsonNode, path: List[PathToken]): Iterator[JsonNode] =
    path match {
      case head :: tail => walk1(node, head).flatMap(walk(_, tail))
      case _            => Iterator.single(node)
    }

  private[this] def walk1(node: JsonNode, query: PathToken): Iterator[JsonNode] =
    query match {
      case RootNode => Iterator.single(rootNode)

      case CurrentNode => Iterator.single(node)

      case Field(name) =>
        if (node.getNodeType == OBJECT) {
          val child = node.get(name)
          if (child == null) {
            Iterator.empty
          } else {
            Iterator.single(child)
          }
        } else {
          Iterator.empty
        }

      case RecursiveField(name) => new RecursiveFieldIterator(node, name)

      case MultiField(fieldNames) =>
        if (node.getNodeType == OBJECT) {
          // don't use collect on iterator with filter causes (executed twice)
          fieldNames.iterator.flatMap(name => Option(node.get(name)))
        } else {
          Iterator.empty
        }

      case AnyField =>
        if (node.getNodeType == OBJECT) {
          node.elements.asScala
        } else {
          Iterator.empty
        }

      case ArraySlice(None, None, 1) =>
        if (node.getNodeType == ARRAY) {
          node.elements.asScala
        } else {
          Iterator.empty
        }

      case ArraySlice(start, stop, step) =>
        if (node.getNodeType == ARRAY) {
          sliceArray(node, start, stop, step)
        } else {
          Iterator.empty
        }

      case ArrayRandomAccess(indices) =>
        if (node.getNodeType == ARRAY) {
          indices.iterator.collect {
            case i if i >= 0 && i < node.size  => node.get(i)
            case i if i < 0 && i >= -node.size => node.get(i + node.size)
          }
        } else {
          Iterator.empty
        }

      case RecursiveFilterToken(filterToken) => new RecursiveDataIterator(node).flatMap(applyFilter(_, filterToken))

      case filterToken: FilterToken => applyFilter(node, filterToken)

      case RecursiveAnyField => new RecursiveNodeIterator(node)
    }

  private[this] def applyFilter(currentNode: JsonNode, filterToken: FilterToken): Iterator[JsonNode] = {

    def resolveSubQuery(node: JsonNode, q: List[AST.PathToken], nextOp: JsonNode => Boolean): Boolean = {
      val it = walk(node, q)
      it.hasNext && nextOp(it.next())
    }

    def applyBinaryOpWithResolvedLeft(node: JsonNode, op: ComparisonOperator, lhsNode: JsonNode, rhs: FilterValue): Boolean =
      rhs match {
        case FilterDirectValue(valueNode) => op(lhsNode, valueNode)
        case SubQuery(q)                  => resolveSubQuery(node, q, op(lhsNode, _))
      }

    def applyBinaryOp(op: ComparisonOperator, lhs: FilterValue, rhs: FilterValue): JsonNode => Boolean =
      lhs match {
        case FilterDirectValue(valueNode) => applyBinaryOpWithResolvedLeft(_, op, valueNode, rhs)
        case SubQuery(q)                  => node => resolveSubQuery(node, q, applyBinaryOpWithResolvedLeft(node, op, _, rhs))
      }

    def elementsToFilter(node: JsonNode): Iterator[JsonNode] =
      node.getNodeType match {
        case ARRAY  => node.elements.asScala
        case OBJECT => Iterator.single(node)
        case _      => Iterator.empty
      }

    def evaluateFilter(filterToken: FilterToken): JsonNode => Boolean =
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

  private[this] def sliceArray(array: JsonNode, start: Option[Int], stop: Option[Int], step: Int): Iterator[JsonNode] = {
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

    val elements: Iterator[JsonNode] = if (step < 0) Iterator.range(array.size - 1, -1, -1).map(array.get) else array.elements.asScala
    val fromStartToEnd = elements.slice(absStart, absEnd)

    if (absStep == 1)
      fromStartToEnd
    else
      fromStartToEnd.grouped(absStep).map(_.head)
  }
}
