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

/**
 * Collect all nodes
 *
 * @param root the tree root
 */
class RecursiveNodeIterator(root: JsonElement[_]) extends RecursiveIterator[Iterator[JsonElement[_]]](root) {

  override protected def visit(it: Iterator[JsonElement[_]]): Unit = {
    while (it.hasNext && !pause) {
      visitNode(it.next())
    }
    if (!pause) {
      stack = stack.tail
    }
  }

  override protected def visitNode(node: JsonElement[_]): Unit =
    node match {
      case obj: ObjectElement =>
        if (obj.nonEmpty) {
          stack = obj.values.map(_._2) :: stack
        }
        nextNode = node
        pause = true
      case array: ArrayElement =>
        if (array.nonEmpty) {
          stack = array.values :: stack
        }
        nextNode = node
        pause = true
      case _ =>
        nextNode = node
        pause = true
    }
}
