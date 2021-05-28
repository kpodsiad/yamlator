
import me.kpodsiad.load.parser._
import me.kpodsiad.{YamlReader, StringYamlReader}
import me.kpodsiad.load.graph.{NodeTransformer, RootNode}
import me.kpodsiad.load.composer.Composer
import org.junit.Test
import org.junit.Assert._

import scala.annotation.tailrec

class ComposerTests {
  final case class Person(name: String, hr: Int, avg: Double) derives Composer
  final case class Dog(name: String, owner: String, age: Int) derives Composer

  @tailrec
  final def getEvents(in: YamlReader, ctx: ParserCtx, acc: List[YamlEvent]): List[YamlEvent] =
    val (event, newCtx) = ParserImpl.getNextEvent(in, ctx)
    if event != StreamEnd then getEvents(in, newCtx, acc :+ event ) else acc :+ event


  @Test def testYamlMapping(): Unit = {
    val yamlMapping =
      """|name: Mark McGwire
         |hr:   65
         |avg:  0.278""".stripMargin

    val events = getEvents(StringYamlReader(yamlMapping), ParserCtx(ParseStreamStart, ParseStreamStart), Nil)
    val node = NodeTransformer.fromEvents(events).asInstanceOf[RootNode]
    val result = summon[Composer[Person]].compose(node.nodes.head)
    assertEquals(result, Right(Person("Mark McGwire", 65, 0.278)))
  }

  @Test def testYamlMapping2(): Unit = {
    val yamlMapping =
      """|name:  Burek
         |owner: Marian
         |age:   13""".stripMargin

    val events = getEvents(StringYamlReader(yamlMapping), ParserCtx(ParseStreamStart, ParseStreamStart), Nil)
    val node = NodeTransformer.fromEvents(events).asInstanceOf[RootNode]
    val result = summon[Composer[Dog]].compose(node.nodes.head)
    assertEquals(result, Right(Dog("Burek", "Marian", 13)))
  }

  @Test def testYamlSequence(): Unit = {
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

    val events = getEvents(StringYamlReader(yamlSequence), ParserCtx(ParseStreamStart, ParseStreamStart), Nil)
    val node = NodeTransformer.fromEvents(events).asInstanceOf[RootNode]
    val result = summon[Composer[List[Person]]].compose(node.nodes.head)
    assertEquals(result, Right(List(Person("Mark McGwire", 65, 0.278), Person("Sammy Sosa", 63, 0.288))))
  }
}