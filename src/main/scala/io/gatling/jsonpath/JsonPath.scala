package io.gatling.jsonpath

import scala.math.abs
import scala.collection.JavaConversions._
import com.fasterxml.jackson.databind.{ JsonNode, ObjectMapper }
import com.fasterxml.jackson.databind.node._
import io.gatling.jsonpath.AST._

case class JPError(reason: String)

object JsonPath {
	val mapper = new ObjectMapper
	val parser = new ThreadLocal[Parser]() {
		override def initialValue() = new Parser
	}

	def compile(query: String): Either[JPError, JsonPath] =
		parser.get.compile(query) match {
			case Parser.Success(q, _) => Right(new JsonPath(q))
			case Parser.NoSuccess(msg, _) => Left(JPError(msg))
		}

	def queryJsonString(query: String, jsonString: String): Either[JPError, Iterator[Any]] =
		compile(query).right.map(_.queryJsonString(jsonString))

	def queryJsonObject(query: String, jsonObject: JsonNode): Either[JPError, Iterator[Any]] =
		compile(query).right.map(_.queryJsonObject(jsonObject))
}

class JsonPath(path: List[PathToken]) {
	import JsonPath.mapper

	def queryJsonObject(jsonObject: JsonNode) = {
		new JsonPathWalker(jsonObject, path).walk
	}

	def queryJsonString(jsonString: String) = {
		new JsonPathWalker(mapper.readTree(jsonString), path).walk
	}
}

class JsonPathWalker(rootNode: JsonNode, fullPath: List[PathToken]) {

	def walk(): Iterator[JsonNode] = walk(rootNode, fullPath)

	// use @tailrec in Scala 2.11, cf: https://github.com/scala/scala/pull/2865
	private[this] def walk(node: JsonNode, path: List[PathToken]): Iterator[JsonNode] =
		path match {
			case head :: tail => walk1(node, head).flatMap(walk(_, tail))
			case Nil => Iterator.single(node)
		}

	private[this] def walk1(node: JsonNode, query: PathToken): Iterator[JsonNode] = {
		query match {
			case RootNode => Iterator.single(rootNode)

			case CurrentNode => Iterator.single(node)

			case Field(name) => node match {
				case obj if obj.isObject =>
					val x = obj.get(name)
					if (x == null) Iterator.empty else Iterator.single(x)
				case _ => Iterator.empty
			}

			case RecursiveField(name) => recFieldFilter(node, name)

			case MultiField(fieldNames) => node match {
				case obj if obj.isObject =>
					fieldNames.iterator.map(obj.get(_)).filter(_ != null)
				case _ => Iterator.empty
			}

			case AnyField => node match {
				case obj if obj.isObject => obj.elements
				case _ => Iterator.empty
			}

			case ArraySlice(None, None, 1) => node match {
				case array if array.isArray => array.elements
				case _ => Iterator.empty
			}

			case ArraySlice(start, stop, step) => node match {
				case array if array.isArray => sliceArray(array.asInstanceOf[ArrayNode], start, stop, step)
				case _ => Iterator.empty
			}

			case ArrayRandomAccess(indices) => node match {
				case array: ArrayNode =>
					indices.iterator
						.collect {
							case i if i >= 0 && i < array.size => array.get(i)
							case i if i < 0 && i >= -array.size => array.get(i + array.size)
						}
				case _ => Iterator.empty
			}

			case filterToken: FilterToken => applyFilter(filterToken, node)

			case RecursiveAnyField => recFieldExplorer(node)
		}
	}

	private[this] def applyFilter(filterToken: FilterToken, node: JsonNode): Iterator[JsonNode] = {

		def resolveFilterToken(node: JsonNode, filter: FilterValue): Option[JsonNode] =
			filter match {
				case JPLong(l) => Some(LongNode.valueOf(l))
				case JPDouble(d) => Some(DoubleNode.valueOf(d))
				case JPString(s) => Some(TextNode.valueOf(s))
				case SubQuery(q) => {
					val it = walk(node, q)
					if (it.hasNext) Some(it.next) else None
				}
			}

		def applyBinaryOp(node: JsonNode, op: ComparisonOperator, lhs: FilterValue, rhs: FilterValue): Boolean = {
			val opEvaluation = for {
				lhsNode <- resolveFilterToken(node, lhs) if lhsNode.isValueNode
				rhsNode <- resolveFilterToken(node, rhs) if rhsNode.isValueNode
			} yield op(lhsNode.asInstanceOf[ValueNode], rhsNode.asInstanceOf[ValueNode])

			opEvaluation.getOrElse(false)
		}

		def elementsToFilter(node: JsonNode): Iterator[JsonNode] =
			node match {
				case node if node.isContainerNode => node.elements
				case _ => Iterator.empty
			}

		def evaluateFilter(filterToken: FilterToken): JsonNode => Boolean =
			filterToken match {
				case HasFilter(subQuery) => {
					(node) => walk(node, subQuery.path).hasNext
				}

				case ComparisonFilter(op, lhs, rhs) => {
					(node) => applyBinaryOp(node, op, lhs, rhs)
				}

				case BooleanFilter(op, filter1, filter2) => {
					val f1 = evaluateFilter(filter1)
					val f2 = evaluateFilter(filter2)
					(node) => op(f1(node), f2(node))
				}
			}

		val filterFunction = evaluateFilter(filterToken)
		elementsToFilter(node).filter(filterFunction)
	}

	def recFieldFilter(node: JsonNode, name: String): Iterator[JsonNode] = {

		node.findValues(name).iterator.filter(!_.isMissingNode)

		// use @tailrec in Scala 2.11, cf: https://github.com/scala/scala/pull/2865
		//		def _recFieldFilter(node: JsonNode): Iterator[JsonNode] =
		//			node match {
		//				case obj: ObjectNode =>
		//					val (filtered, toExplore) = obj.fields.partition(_.getKey == name)
		//					filtered.map(_.getValue) ++ toExplore.flatMap(e => _recFieldFilter(e.getValue))
		//				case array: ArrayNode => array.elements.flatMap(_recFieldFilter)
		//				case _ => Iterator.empty
		//			}
		//
		//		_recFieldFilter(node)
	}

	def recFieldExplorer(node: JsonNode): Iterator[JsonNode] =
		node match {
			case obj if obj.isObject =>
				obj.elements ++ obj.elements.flatMap(recFieldExplorer)
			case array if array.isArray =>
				array.elements.flatMap(recFieldExplorer)
			case _ => Iterator.empty
		}

	private[this] def sliceArray(array: ArrayNode, start: Option[Int], stop: Option[Int], step: Int): Iterator[JsonNode] = {
		val size = array.size

		def lenRelative(x: Int) = if (x >= 0) x else size + x
		def stepRelative(x: Int) = if (step >= 0) x else -1 - x
		def relative = lenRelative _ compose stepRelative _
		val absStart = start.map(relative).getOrElse(0)
		val absEnd = stop.map(relative).getOrElse(size)
		val absStep = abs(step)

		val elts: Iterator[JsonNode] = if (step < 0) array.toBuffer.reverseIterator else array.iterator
		val fromStartToEnd = elts.slice(absStart, absEnd)

		if (absStep != 1)
			fromStartToEnd.grouped(absStep).map(_.head)
		else
			fromStartToEnd
	}
}
