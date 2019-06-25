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

/**
 * Collect all nodes data (objects and leaves)
 *
 * @param root the tree root
 */
class RecursiveDataIterator(root: JsValue) extends RecursiveIterator[Iterator[JsValue]](root) {

  override protected def visit(it: Iterator[JsValue]): Unit = {
    while (it.hasNext && !pause) {
      visitNode(it.next())
    }
    if (!pause) {
      stack = stack.tail
    }
  }

  override protected def visitNode(node: JsValue): Unit =
    node match {
      case obj: JsObject =>
        if (obj.value.nonEmpty) {
          // only non empty objects
          val it = obj.value.values.iterator
          stack = it :: stack
          nextNode = node
          pause = true
        }
      case array: JsArray =>
        val it = array.value.iterator
        stack = it :: stack
        visit(it)
      case _ =>
        nextNode = node
        pause = true
    }
}
