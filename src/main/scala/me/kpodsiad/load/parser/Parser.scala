package me.kpodsiad.load.parser

import me.kpodsiad.YamlReader

case class ParserCtx(
  current: ParserState,
  state: ParserState
)

trait Parser {
  def getNextEvent(in: YamlReader, ctx: ParserCtx): (YamlEvent, ParserCtx)
  def getEvents(in: YamlReader): Seq[YamlEvent]
}

/** Valid sequence of events should obey following grammar so parser states should mirror that grammar
  stream ::= STREAM-START document* STREAM-END
  document ::= DOCUMENT-START node DOCUMENT-END
  node ::= ALIAS | SCALAR | sequence | mapping
  sequence ::= SEQUENCE-START node* SEQUENCE-END
  mapping ::= MAPPING-START (node node)* MAPPING-END
 */
sealed trait ParserState
case object EndState extends ParserState
/**
 * Parser in state X expects to parse X event
 */
case object ParseStreamStart extends ParserState
case object ParseStreamEnd extends ParserState
case object ParseDocumentStart extends ParserState
case object ParseDocumentEnd extends ParserState
case object ParseNode extends ParserState
case object ParseMapping extends ParserState
case object ParseSequenceStart extends ParserState
case object ParseSequenceEnd extends ParserState
case object ParseFirstKey extends ParserState
case object ParseKey extends ParserState
case object ParseValue extends ParserState
case object ParseScalar extends ParserState
