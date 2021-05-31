import me.kpodsiad.load.composer.Composer
import me.kpodsiad.load.encoder.YamlWriter

final case class Person(name: String, hr: Int, avg: Double) derives Composer, YamlWriter
final case class Dog(name: String, owner: String, age: Int) derives Composer