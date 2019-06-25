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

import play.api.libs.json.{ JsArray, JsObject, JsValue }

sealed trait VisitedIterator {
  def hasNext: Boolean
}

case class VisitedObject(it: Iterator[(String, JsValue)]) extends VisitedIterator {
  override def hasNext: Boolean = it.hasNext
}

case class VisitedArray(it: Iterator[JsValue]) extends VisitedIterator {
  override def hasNext: Boolean = it.hasNext
}

/**
 * Collect all first nodes in a branch with a given name
 *
 * @param root the tree root
 * @param name the searched name
 */
class RecursiveFieldIterator(root: JsValue, name: String) extends RecursiveIterator[VisitedIterator](root) {

  override def visit(t: VisitedIterator): Unit = t match {
    case VisitedObject(it) => visitObject(it)
    case VisitedArray(it)  => visitArray(it)
  }

  private def visitObject(it: Iterator[(String, JsValue)]): Unit = {
    while (it.hasNext && !pause) {
      it.next() match {
        case (key, value) if key == name =>
          nextNode = value
          pause = true
        case (_, value) => visitNode(value)
      }
    }
    if (!pause) {
      stack = stack.tail
    }
  }

  private def visitArray(it: Iterator[JsValue]): Unit = {
    while (it.hasNext && !pause) {
      visitNode(it.next())
    }
    if (!pause) {
      stack = stack.tail
    }
  }

  protected def visitNode(node: JsValue): Unit =
    node match {
      case obj: JsObject =>
        val it = obj.value.iterator
        stack = VisitedObject(it) :: stack
        visitObject(it)
      case array: JsArray =>
        val it = array.value.iterator
        stack = VisitedArray(it) :: stack
        visitArray(it)
      case _ =>
    }
}
