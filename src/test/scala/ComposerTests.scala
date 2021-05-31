
import me.kpodsiad.load.parser._
import me.kpodsiad.{YamlReader, StringYamlReader}
import me.kpodsiad.load.graph.{NodeTransformer, RootNode}
import me.kpodsiad.load.composer.Composer
import me.kpodsiad.load.composer.Composer.as
import org.junit.Test
import org.junit.Assert._

import scala.annotation.tailrec

class ComposerTests {
  @Test def testYamlMapping(): Unit = {
    val yamlMapping =
      """|name: Mark McGwire
         |hr:   65
         |avg:  0.278""".stripMargin

    val events = ParserImpl.getEvents(StringYamlReader(yamlMapping))
    val node = NodeTransformer.fromEvents(events).asInstanceOf[RootNode]
    val result = node.nodes.head.as[Person]
    assertEquals(result, Right(Person("Mark McGwire", 65, 0.278)))
  }

  @Test def testYamlMapping2(): Unit = {
    val yamlMapping =
      """|name:  Burek
         |owner: Marian
         |age:   13""".stripMargin

    val events = ParserImpl.getEvents(StringYamlReader(yamlMapping))
    val node = NodeTransformer.fromEvents(events).asInstanceOf[RootNode]
    val result = node.nodes.head.as[Dog]
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

    val events = ParserImpl.getEvents(StringYamlReader(yamlSequence))
    val node = NodeTransformer.fromEvents(events).asInstanceOf[RootNode]
    val result = node.nodes.head.as[List[Person]]
    assertEquals(result, Right(List(Person("Mark McGwire", 65, 0.278), Person("Sammy Sosa", 63, 0.288))))
  }
}