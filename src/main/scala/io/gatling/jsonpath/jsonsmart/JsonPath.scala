package io.gatling.jsonpath.jsonsmart

import java.util.{ List => JList, Map => JMap }
import io.gatling.jsonpath.{ Parser, ComparisonOperator }
import io.gatling.jsonpath.AST._
import net.minidev.json.JSONValue
import scala.math.abs
import scala.collection.JavaConversions.{ asScalaIterator, asScalaBuffer }

case class JPError(val reason: String)

object JsonPath {
	val parser = new ThreadLocal[Parser]() {
		override def initialValue() = new Parser
	}

	def compile(query: String): Either[JPError, JsonPath] = {
		val compileResult = parser.get.compile(query)
		compileResult.map((q) => Right(new JsonPath(q))).getOrElse(Left(JPError(compileResult.toString)))
	}

	def queryJsonString(query: String, jsonString: String): Either[JPError, Iterator[Any]] =
		compile(query).right.map(_.queryJsonString(jsonString))

	def queryJsonObject(query: String, jsonObject: Any): Either[JPError, Iterator[Any]] =
		compile(query).right.map(_.queryJsonObject(jsonObject))
}

class JsonPath(val path: List[PathToken]) {
	def queryJsonObject(jsonObject: Any) = {
		new JsonPathWalker(jsonObject, path).walk
	}

	def queryJsonString(jsonString: String) = {
		new JsonPathWalker(JSONValue.parse(jsonString), path).walk
	}
}

class JsonPathWalker(val rootNode: Any, val fullPath: List[PathToken]) {

	def walk(): Iterator[Any] = walk(rootNode, fullPath)

	// use @tailrec in Scala 2.11, cf: https://github.com/scala/scala/pull/2865
	private[this] def walk(node: Any, path: List[PathToken]): Iterator[Any] =
		path match {
			case head :: tail => walk1(node, head).flatMap(walk(_, tail))
			case Nil => Iterator.single(node)
		}

	private[this] def walk1(node: Any, query: PathToken): Iterator[Any] = {
		query match {
			case RootNode() => Iterator.single(rootNode)

			case CurrentNode() => Iterator.single(node)

			case Field(name, false) => node match {
				case obj: JMap[_, _] if obj.containsKey(name) =>
					Iterator.single(obj.get(name))
				case _ => Iterator.empty
			}

			case Field(name, true) => recFieldFilter(node, name)

			case MultiField(fieldNames) => node match {
				case obj: JMap[_, _] =>
					fieldNames.iterator.collect { case fieldName if obj.containsKey(fieldName) => obj.get(fieldName) }
				case _ => Iterator.empty
			}

			case AnyField(false) => node match {
				case obj: JMap[_, _] => obj.values.iterator
				case _ => Iterator.empty
			}

			case ArraySlice(None, None, 1) => node match {
				case array: JList[_] => array.iterator
				case _ => Iterator.empty
			}

			case ArraySlice(start, stop, step) => node match {
				case array: JList[_] => sliceArray(array, start, stop, step)
				case _ => Iterator.empty
			}

			case ArrayRandomAccess(indices) => node match {
				case array: JList[_] =>
					indices.iterator
						.collect {
							case i if i >= 0 && i < array.size => array.get(i)
							case i if i < 0 && i >= -array.size => array.get(i + array.size)
						}
				case _ => Iterator.empty
			}

			case filterToken: FilterToken => applyFilter(filterToken, node)

			case AnyField(true) => recFieldExplorer(node)
		}
	}

	private[this] def applyFilter(filterToken: FilterToken, node: Any): Iterator[Any] = {

		def resolveFilterToken(node: Any, filter: FilterValue): Option[Any] =
			filter match {
				case JPLong(l) => Some(l)
				case JPDouble(d) => Some(d)
				case JPString(s) => Some(s)
				case SubQuery(q) => {
					val it = walk(node, q)
					if (it.hasNext) Some(it.next) else None
				}
			}

		def applyBinaryOp(node: Any, op: ComparisonOperator, lhs: FilterValue, rhs: FilterValue): Boolean = {
			val opEvaluation = for {
				lhsNode <- resolveFilterToken(node, lhs)
				rhsNode <- resolveFilterToken(node, rhs)
			} yield op(lhsNode, rhsNode)

			opEvaluation.getOrElse(false)
		}

		def elementsToFilter(node: Any): Iterator[Any] =
			node match {
				case array: JList[_] => array.iterator
				case obj: JMap[_, _] => obj.values.iterator
				case _ => Iterator.empty
			}

		def evaluateFilter(filterToken: FilterToken): Any => Boolean =
			filterToken match {
				case HasFilter(subQuery) => {
					(node: Any) => walk(node, subQuery.path).hasNext
				}

				case ComparisonFilter(op, lhs, rhs) => {
					(node: Any) => applyBinaryOp(node, op, lhs, rhs)
				}

				case BooleanFilter(op, filter1, filter2) => {
					val f1 = evaluateFilter(filter1)
					val f2 = evaluateFilter(filter2)
					(node: Any) => op(f1(node), f2(node))
				}
			}

		val filterFunction = evaluateFilter(filterToken)
		elementsToFilter(node).filter(filterFunction)
	}

	// use @tailrec in Scala 2.11, cf: https://github.com/scala/scala/pull/2865
	def recFieldFilter(node: Any, name: String): Iterator[Any] = {
		def _recFieldFilter(node: Any): Iterator[Any] =
			node match {
				case obj: JMap[_, _] =>
					val (filtered, toExplore) = obj.entrySet.iterator.partition(_.getKey == name)
					filtered.map(_.getValue) ++ toExplore.flatMap(e => _recFieldFilter(e.getValue))
				case list: JList[_] => list.iterator.flatMap(_recFieldFilter)
				case _ => Iterator.empty
			}

		_recFieldFilter(node)
	}

	def recFieldExplorer(node: Any): Iterator[Any] =
		node match {
			case obj: JMap[_, _] =>
				obj.values.iterator ++ obj.values.iterator.flatMap(recFieldExplorer)
			case list: JList[_] =>
				list.iterator.flatMap(recFieldExplorer)
			case _ => Iterator.empty
		}

	private[this] def sliceArray(array: JList[_], start: Option[Int], stop: Option[Int], step: Int): Iterator[Any] = {
		val size = array.size

		def lenRelative(x: Int) = if (x >= 0) x else size + x
		def stepRelative(x: Int) = if (step >= 0) x else -1 - x
		def relative = lenRelative _ compose stepRelative _
		val absStart = start.map(relative).getOrElse(0)
		val absEnd = stop.map(relative).getOrElse(size)
		val absStep = abs(step)

		val elts: Iterator[Any] = if (step < 0) array.reverseIterator else array.iterator
		val fromStartToEnd = elts.slice(absStart, absEnd)

		if (absStep != 1)
			fromStartToEnd.grouped(absStep).map(_.head)
		else
			fromStartToEnd
	}
}
