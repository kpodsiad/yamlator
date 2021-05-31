package me.kpodsiad.load.encoder

import me.kpodsiad.load.composer.{Composer, InvalidType}
import me.kpodsiad.load.encoder
import me.kpodsiad.load.graph.NodeMapping

import scala.deriving._
import scala.compiletime._
import scala.deriving.Mirror

trait YamlWriter[T] {
   def toYaml(obj: T, indent: Int = 0, base: Int = 2): String
}

object YamlWriter:
  extension [T: YamlWriter](t: T)
    def toYaml(indent: Int = 0, base: Int = 2): String = summon[YamlWriter[T]].toYaml(t)
    
  given YamlWriter[Int] with
    override def toYaml(obj: Int, indent: Int = 0, base: Int = 2): String = obj.toString

  given YamlWriter[String] with
    override def toYaml(obj: String, indent: Int = 0, base: Int = 2): String = obj.toString

  given YamlWriter[Double] with
    override def toYaml(obj: Double, indent: Int = 0, base: Int = 2): String = obj.toString

  given seqWriter[T](using writer: YamlWriter[T]): YamlWriter[List[T]] with
    override def toYaml(elements: List[T], indent: Int = 0, base: Int = 2): String = {
      val values = elements.map(x => writer.toYaml(x, indent + 1, base))
      values.mkString("-\n","\n-\n", "\n")
    }


  private def iterator[T](p: T) = p.asInstanceOf[Product].productIterator

  inline def derived[T](using m: Mirror.Of[T]): YamlWriter[T] = {
    val fromWriterElems = summonAll[m.MirroredElemTypes]
    val elemLabels = getElemLabels[m.MirroredElemLabels]
    inline m match {
      case p: Mirror.ProductOf[T] =>
        new YamlWriter[T] {
          override def toYaml(obj: T, indent: Int = 0, base: Int = 2): String = {
            val products = iterator(obj)
            val values = elemLabels.zip(products).zip(fromWriterElems).map { case ((label, value), yamlWriter) =>
              s"${" ".repeat(indent*base)}$label: ${yamlWriter.asInstanceOf[YamlWriter[Any]].toYaml(value, indent, base)}"
            }
            values.mkString("\n")
          }
        }
      case _ =>  sys.error(s"Can not parse ${m.getClass.getSimpleName} class")
    }
  }

  inline def summonAll[T <: Tuple]: List[YamlWriter[_]] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[YamlWriter[t]] :: summonAll[ts]
  }

  inline def getElemLabels[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (head *: tail) => constValue[head].toString :: getElemLabels[tail]
  }