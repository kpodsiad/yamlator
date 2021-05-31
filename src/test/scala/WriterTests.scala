import org.junit.Test
import org.junit.Assert._
import me.kpodsiad.load.encoder.YamlWriter
import me.kpodsiad.load.encoder.YamlWriter.toYaml

class WriterTests {
  val sammy = Person(name = "Sammy Sosa", hr = 63, avg = 0.288)
  val mark = Person(name = "Mark McGwire", hr = 65, avg = 0.278)

  @Test def testCaseClassWrite(): Unit = {
    val markString =
      """|name: Mark McGwire
         |hr: 65
         |avg: 0.278""".stripMargin
    assert(mark.toYaml() == markString)
  }

  @Test def testSeqWrite(): Unit = {
    val string =
      """-
        |  name: Mark McGwire
        |  hr: 65
        |  avg: 0.278
        |-
        |  name: Sammy Sosa
        |  hr: 63
        |  avg: 0.288
        |""".stripMargin
    assert(List(mark, sammy).toYaml() == string)
  }
}
