package me.kpodsiad

import me.kpodsiad.load.parser._

import scala.annotation.tailrec
import scala.deriving.Mirror

object Main extends App {
  case class Person(name: String, hr: Int, avg: Double)

  val yaml =
    """name: Mark McGwire
      |hr:   65
      |avg:  0.278
      |""".stripMargin

  @tailrec
  def printEvents(in: YamlReader, ctx: ParserCtx): Unit =
    val (event, newCtx) = ParserImpl.getNextEvent(in, ctx)
    println(event)
    if event != StreamEnd then printEvents(in, newCtx) else ()

  printEvents(StringYamlReader(yaml), ParserCtx(ParseStreamStart))
}
