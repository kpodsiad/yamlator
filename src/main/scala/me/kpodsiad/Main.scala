package me.kpodsiad

import me.kpodsiad.load.graph.NodeTransformer
import me.kpodsiad.load.parser._

import scala.annotation.tailrec
import scala.deriving.Mirror

object Main extends App {
  case class Person(name: String, hr: Int, avg: Double)

  val yaml =
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

  val events = renderEvents(StringYamlReader(yaml), ParserCtx(ParseStreamStart, ParseStreamStart), Nil)
  events.foreach(println)

  val node = NodeTransformer.fromEvents(events)
  println(node)
}
