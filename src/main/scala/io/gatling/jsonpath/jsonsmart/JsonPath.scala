package io.gatling.jsonpath.jsonsmart

import java.util.{ List => JList, Map => JMap }
import scala.math.abs
import net.minidev.json.JSONValue
import io.gatling.jsonpath._
import scala.collection.JavaConversions.{ asScalaIterator, asScalaBuffer }

case class JPError(val reason: String)

object JsonPath {
	val parser = Parser

	def compile(query: String): Either[JPError, JsonPath] = {
		val compileResult = parser.compile(query)
		compileResult.map((q) => Right(new JsonPath(q))).getOrElse(Left(JPError(compileResult.toString)))
	}

	def query(query: String, json: String): Either[JPError, Seq[Any]] = {
		compile(query).right.map(_.query(json))
	}

	def query(query: String, json: Any): Either[JPError, Seq[Any]] = {
		compile(query).right.map(_.query(json))
	}
}

class JsonPath(val path: List[PathToken]) {

	def query(json: Any) = {
		walk(json, path).toVector
	}

	def query(json: String) = {
		walk(JSONValue.parse(json), path).toVector
	}

	private[this] def walk(node: Any, path: List[PathToken]): Iterator[Any] = {
		if (path.isEmpty)
			Iterator.single(node)
		else {
			val head :: tail = path
			val nodes = walk1(node, head)
			nodes.flatMap(walk(_, tail))
		}
	}

	private[this] def walk1(node: Any, query: PathToken): Iterator[Any] = {
		query match {
			case Root() => Iterator.single(node)

			case CurrentObject() => Iterator.single(node)

			case Field(name, false) => node match {
				case obj: JMap[_, _] if (obj.containsKey(name)) =>
					Iterator.single(obj.get(name))
				case _ => Iterator.empty
			}

			case Field(name, true) => recFieldFilter(node, name)

			case MultiField(fieldNames) => node match {
				case obj: JMap[_, _] =>
					fieldNames.iterator.filter(obj.containsKey(_)).map(obj.get(_))
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
					indices.iterator.filter(i => i >= 0 && i < array.size).map(array.get(_))
				case _ => Iterator.empty
			}

			case HasFilter(subQuery) => {
				val elements: Iterator[Any] = node match {
					case array: JList[_] => array.iterator
					case obj: JMap[_, _] => obj.values.iterator
					case _ => Iterator.empty
				}

				elements.filter(walk(_, subQuery.path).hasNext)
			}
			case BinaryOpFilter(op, lhs, rhs) => {
				val elements: Iterator[Any] = node match {
					case array: JList[_] => array.iterator
					case obj: JMap[_, _] => obj.values.iterator
					case _ => Iterator.empty
				}

				elements.filter(applyBinaryOp(_, op, lhs, rhs))
			}

			case AnyField(true) => recFieldExplorer(node)

			case _ => Iterator.empty
		}
	}

	/* Can we make it tail rec ? */
	def recFieldFilter(node: Any, name: String): Iterator[Any] = {
		def _recFieldFilter(node: Any): Iterator[Any] =
			node match {
				case obj: JMap[_, _] =>
					val (filtered, toExplore) = obj.entrySet().iterator.partition(e => e.getKey == name)
					filtered.map(_.getValue) ++ toExplore.flatMap(e => _recFieldFilter(e.getValue))
				case list: JList[_] => list.iterator.flatMap(_recFieldFilter(_))
				case _ => Iterator.empty
			}

		_recFieldFilter(node)
	}

	def recFieldExplorer(node: Any): Iterator[Any] =
		node match {
			case obj: JMap[_, _] =>
				obj.values.iterator ++ obj.values.iterator.flatMap(recFieldExplorer(_))
			case list: JList[_] =>
				list.iterator.flatMap(recFieldExplorer(_))
			case _ => Iterator.empty
		}

	private [this] def sliceArray(array: JList[_], start: Option[Int], stop: Option[Int], step: Int): Iterator[Any] = {
		val size = array.size

		def lenRelative(x: Int) = if (x >= 0) x else size + x
		def stepRelative(x: Int) = if (step >= 0) x else -1 - x
		def relative = lenRelative _ compose stepRelative _
		val absStart = start.map(relative(_)).getOrElse(0)
		val absEnd = stop.map(relative(_)).getOrElse(size)
		val absStep = abs(step)

		val elts: Iterator[Any] = if (step < 0) array.toBuffer.reverseIterator else array.iterator
		val fromStartToEnd = elts.slice(absStart, absEnd)

		if (absStep != 1)
			fromStartToEnd.grouped(absStep).map(_.head)
		else
			fromStartToEnd
	}

	private[this] def applyBinaryOp(node: Any, op: String, lhs: FilterToken, rhs: FilterToken): Boolean = {
		val opEvaluation = for (
			lhsNode <- resolveFilterToken(node, lhs);
			rhsNode <- resolveFilterToken(node, rhs)
		) yield OrderedOperator(op, lhsNode, rhsNode)

		opEvaluation.getOrElse(false)
	}

	private[this] def resolveFilterToken(node: Any, filter: FilterToken): Option[Any] =
		filter match {
			case JPLong(l) => Some(l)
			case JPDouble(d) => Some(d)
			case JPString(s) => Some(s)
			case SubQuery(q) => {
				val it = walk(node, q)
				if (it.hasNext) Some(it.next) else None
			}
		}
}

object OrderedOperator {

	def isNumber(a: Any) = isIntegralNumber(a) || isFloatingPointNumber(a)

	def isIntegralNumber(a: Any) = a.isInstanceOf[Int] || a.isInstanceOf[Long]
	def asIntegralNumber(a: Any): Option[Long] = a match {
		case a: Integer => Some(a.toLong)
		case a: Long => Some(a)
		case _ => None
	}

	def isFloatingPointNumber(a: Any) = a.isInstanceOf[Float] || a.isInstanceOf[Double]
	def asFloatingPointNumber(a: Any): Option[Double] = a match {
		case a: Int => Some(a.toDouble)
		case a: Long => Some(a.toDouble)
		case a: Float => Some(a.toDouble)
		case a: Double => Some(a)
		case _ => None
	}

	def apply(op: String, lhs: Any, rhs: Any): Boolean =
		(lhs, rhs) match {
			case (s1: String, s2: String) => eval(op, s1, s2)
			case (i1, i2) if (isNumber(lhs) && isNumber(rhs)) =>
				if (isIntegralNumber(lhs) && isIntegralNumber(rhs))
					eval(op, asIntegralNumber(i1), asIntegralNumber(i2))
				else
					eval(op, asFloatingPointNumber(i1), asFloatingPointNumber(i2))
			case _ => false
		}

	def eval[T <% Ordered[T]](op: String, lhs: T, rhs: T): Boolean =
		op match {
			case "==" => (lhs == rhs)
			case ">=" => (lhs >= rhs)
			case ">" => (lhs > rhs)
			case "<=" => (lhs <= rhs)
			case "<" => (lhs < rhs)
		}
}