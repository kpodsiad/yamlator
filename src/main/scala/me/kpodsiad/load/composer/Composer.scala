package me.kpodsiad.load.composer

import scala.deriving._
import scala.compiletime._
import me.kpodsiad.load.graph.{Node, NodeMapping, NodeSequence}

sealed trait ComposerError
case class MissingKey(msg: String) extends ComposerError
case class InvalidValue(msg: String) extends ComposerError
case class InvalidType(msg: String) extends ComposerError
case object DevError extends ComposerError

trait Composer[T] {
  def compose(node: Node): Either[List[ComposerError], T]
}

trait FromMap[T] {
  def fromMap(key: String, map: Map[String, String]): Either[ComposerError, T]
}

private object FromMap:
  given FromMap[String] = new FromMap[String] {
    def fromMap(key: String, map: Map[String, String]): Either[ComposerError, String]  = map
      .get(key)
      .map(x => Right(x))
      .getOrElse(Left(MissingKey(s"Key ${key} is missing in yaml file")))
  }

  given FromMap[Int] = new FromMap[Int] {
    def fromMap(key: String, map: Map[String, String]): Either[ComposerError, Int] = map
      .get(key)
      .map(x => Right(x.toInt))
      .getOrElse(Left(MissingKey(s"Key ${key} is missing in yaml file")))
  }

  given FromMap[Double] = new FromMap[Double] {
    def fromMap(key: String, map: Map[String, String]): Either[ComposerError, Double]  = map
      .get(key)
      .map(x => Right(x.asInstanceOf[String].toDouble))
      .getOrElse(Left(MissingKey(s"Key ${key} is missing in yaml file")))
  }

object Composer:
  given [T](using Composer[T]): Composer[List[T]] = new Composer[List[T]] {
    override def compose(node: Node): Either[List[ComposerError], List[T]] = {
      node match {
        case NodeSequence(nodes) =>
          val composer: Composer[T] = summon[Composer[T]]
          val (left, right) = nodes.map(composer.compose(_)).partitionMap {
            case l @ Left(error) => l
            case r @ Right(value) => r
          }
          if left.nonEmpty then Left(left.flatten)
          else Right(right)
        case _ =>  Left(List(InvalidType(s"Expected NodeSequence cannot parse ${node.getClass.getSimpleName}")))
      }
    }
  }

  inline def derived[T](using m: Mirror.Of[T]): Composer[T] = {
    val fromMapElems = summonAll[m.MirroredElemTypes]
    val elemLabels = getElemLabels[m.MirroredElemLabels]
    inline m match {
      case p: Mirror.ProductOf[T] =>
        new Composer[T] {
          override def compose(node: Node): Either[List[ComposerError], T] =
            val valuesMap = node.asInstanceOf[NodeMapping].map.map(e => e.key.value -> e.value.value).toMap
            val values = elemLabels.zip(fromMapElems).map { case (label, fromMap) =>
              fromMap.fromMap(label, valuesMap)
            }
            val (left, right) = values.partitionMap {
              case l @ Left(error) => l
              case r @ Right(value) => r
            }
            if left.nonEmpty then Left(left)
            else Right(p.fromProduct(Tuple.fromArray(right.toArray)))
        }
      case s: Mirror.SumOf[T]     => ???
    }
  }

  inline def summonAll[T <: Tuple]: List[FromMap[_]] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[FromMap[t]] :: summonAll[ts]
  }

  inline def getElemLabels[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (head *: tail) => constValue[head].toString :: getElemLabels[tail]
  }
