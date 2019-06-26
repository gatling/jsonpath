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

package io.gatling.jsonpath.jackson

import scala.collection.JavaConverters._
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node._
import io.gatling.jsonpath._

object JsonPath extends BaseJsonPath[JsonNode] {
  override protected def mapJsonObject(jsonObject: JsonNode): JsonElement[_] = JacksonElement(jsonObject)
}

private object JacksonElement {

  def apply(value: JsonNode): JsonElement[_] = value match {
    case obj: ObjectNode                          => JacksonObjectElement(obj)
    case array: ArrayNode                         => JacksonArrayElement(array)
    case num: NumericNode if num.isIntegralNumber => LongValue(num.longValue())
    case num: NumericNode                         => DoubleValue(num.doubleValue())
    case bool: BooleanNode                        => BooleanValue(bool.booleanValue())
    case str: TextNode                            => StringValue(str.textValue())
    case _: NullNode                              => NullValue
  }

  case class JacksonObjectElement(obj: ObjectNode) extends ObjectElement {
    override def contains(key: String): Boolean = obj.get(key) != null

    override def apply(key: String): JsonElement[_] = JacksonElement(obj.get(key))

    override def values: Iterator[(String, JsonElement[_])] =
      obj.fields().asScala.map(entry => (entry.getKey, JacksonElement(entry.getValue)))

    override def size: Int = obj.size

    override def isEmpty: Boolean = obj.size == 0
  }

  case class JacksonArrayElement(array: ArrayNode) extends ArrayElement {
    override def values: Iterator[JsonElement[_]] = array.iterator.asScala.map(JacksonElement(_))

    override def apply(index: Int): JsonElement[_] = JacksonElement(array.get(index))

    override def size: Int = array.size

    override def isEmpty: Boolean = array.size == 0
  }

}
