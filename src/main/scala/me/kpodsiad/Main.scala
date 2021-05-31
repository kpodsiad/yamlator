package me.kpodsiad

import me.kpodsiad.load.composer.{Composer, ComposerError}
import me.kpodsiad.load.composer.Composer.as
import me.kpodsiad.load.graph.{Node, NodeTransformer, RootNode}
import me.kpodsiad.load.parser._
import me.kpodsiad.load.encoder._

import scala.annotation.tailrec
import scala.deriving.Mirror

object Main extends App {
  final case class Person(name: String, hr: Int, avg: Double) derives Composer, YamlWriter
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

  val events = ParserImpl.getEvents(StringYamlReader(yamlSequence))
  events.foreach(println)
  val node = NodeTransformer.fromEvents(events).asInstanceOf[RootNode]
  println(node)
  println(node.nodes.head.as[List[Person]])
  
  val personToYaml = Person(name = "Sammy Sosa", hr = 63, avg = 0.288)
  val personToYamlMark = Person(name = "Mark McGwire", hr = 65, avg = 0.278)
  println(s"\nperson to yaml - ${personToYaml.toString}\n")
  val yamlPerson = summon[YamlWriter[Person]].toYaml(personToYaml)
  println(yamlPerson)
  println(s"\nperson to yaml - ${List(personToYaml, personToYaml).toString}\n")
  val yamlListPerson = summon[YamlWriter[List[Person]]].toYaml(List(personToYaml, personToYaml))
  println(yamlListPerson)
}
