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

package io.gatling.jsonpath.play

import play.api.libs.json._
import io.gatling.jsonpath._

object JsonPath extends BaseJsonPath[JsValue] {
  override protected def mapJsonObject(jsonObject: JsValue): JsonElement[_] = PlayJsonElement(jsonObject)
}

private object PlayJsonElement {

  def apply(value: JsValue): JsonElement[_] = value match {
    case obj: JsObject                    => PlayObjectElement(obj)
    case array: JsArray                   => PlayArrayElement(array)
    case JsNumber(num) if num.isValidLong => LongValue(num.longValue())
    case JsNumber(num)                    => DoubleValue(num.doubleValue())
    case JsBoolean(bool)                  => BooleanValue(bool)
    case JsString(str)                    => StringValue(str)
    case JsNull                           => NullValue
  }

  case class PlayObjectElement(obj: JsObject) extends ObjectElement {
    override def contains(key: String): Boolean = obj.keys.contains(key)
    override def apply(key: String): JsonElement[_] = PlayJsonElement(obj.value(key))
    override def values: Iterator[(String, JsonElement[_])] =
      obj.value.iterator.map { case (key, value) => (key, PlayJsonElement(value)) }
    override def size: Int = obj.value.size
    override def isEmpty: Boolean = obj.value.isEmpty
  }

  case class PlayArrayElement(array: JsArray) extends ArrayElement {
    override def values: Iterator[JsonElement[_]] = array.value.iterator.map(PlayJsonElement(_))
    override def apply(index: Int): JsonElement[_] = PlayJsonElement(array.value(index))
    override def size: Int = array.value.size
    override def isEmpty: Boolean = array.value.isEmpty
  }

}
