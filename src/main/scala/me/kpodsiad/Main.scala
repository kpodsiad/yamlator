package me.kpodsiad

import me.kpodsiad.load.composer.Composer
import me.kpodsiad.load.graph.NodeTransformer
import me.kpodsiad.load.graph.RootNode
import me.kpodsiad.load.parser._

import scala.annotation.tailrec
import scala.deriving.Mirror

object Main extends App {
  final case class Person(name: String, hr: Int, avg: Double) derives Composer
  val yamlSequence =
    """-
      |  name: Mark McGwire
      |  hr:   65
      |  avg:  0.278
      |-
      |  name: Sammy Sosa
      |  hr:   63
      |  avg:  0.288
      |""".stripMargin

  @tailrec
  def renderEvents(in: YamlReader, ctx: ParserCtx, acc: List[YamlEvent]): List[YamlEvent] =
    val (event, newCtx) = ParserImpl.getNextEvent(in, ctx)
    if event != StreamEnd then renderEvents(in, newCtx, acc :+ event ) else acc :+ event

  val events = renderEvents(StringYamlReader(yamlSequence), ParserCtx(ParseStreamStart, ParseStreamStart), Nil)
  events.foreach(println)
  val node = NodeTransformer.fromEvents(events).asInstanceOf[RootNode]
  println(node)
  val result = summon[Composer[List[Person]]].compose(node.nodes.head)
  println(result)
}
