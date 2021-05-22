package me.kpodsiad.load.parser

/** Valid sequence of events should obey following grammar
  stream ::= STREAM-START document* STREAM-END
  document ::= DOCUMENT-START node DOCUMENT-END
  node ::= ALIAS | SCALAR | sequence | mapping
  sequence ::= SEQUENCE-START node* SEQUENCE-END
  mapping ::= MAPPING-START (node node)* MAPPING-END
*/

sealed trait YamlEvent
case object YamlEventEnd extends YamlEvent
sealed trait Stream extends YamlEvent
case object StreamStart extends Stream
case object StreamEnd extends Stream

sealed trait Document extends YamlEvent
case object DocumentStart extends Document
case object DocumentEnd extends Document

sealed trait Node extends YamlEvent
//  case object Alias extends Node
case class Scalar(value: String) extends Node

//  sealed trait Sequence extends Node
//  case object SequenceStart extends Sequence
//  case object SequenceEnd extends Sequence

sealed trait Mapping extends Node
case object MappingStart extends Mapping
case object MappingEnd extends Mapping
