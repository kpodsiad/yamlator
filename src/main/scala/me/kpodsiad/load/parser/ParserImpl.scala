package me.kpodsiad.load.parser

import me.kpodsiad.YamlReader

import scala.annotation.tailrec

object ParserImpl extends Parser:
  override def getEvents(in: YamlReader): List[YamlEvent] = {
      @tailrec
      def loop(ctx: ParserCtx, acc: List[YamlEvent]): List[YamlEvent] = {
        val (event, newCtx) = ParserImpl.getNextEvent(in, ctx)
        if event != StreamEnd then loop(newCtx, acc :+ event ) else acc :+ event
      }
      loop(ParserCtx(ParseStreamStart, ParseStreamStart), Nil)
  }

  override def getNextEvent(in: YamlReader, ctx: ParserCtx): (YamlEvent, ParserCtx) = {
    if in.hasNext then skipUntilNextToken(in)
    ctx.state match {
      case EndState => (YamlEventEnd, ctx)
      case ParseStreamStart => (StreamStart, ctx.copy(state = ParseDocumentStart))
      case ParseStreamEnd => (StreamEnd, ctx.copy(state = EndState))
      case ParseDocumentStart => (DocumentStart, ctx.copy(state = ParseNode))
      case ParseDocumentEnd => (DocumentEnd, ctx.copy(state = ParseStreamEnd))
      case ParseSequenceEnd => (SequenceEnd, ctx.copy(state = ParseDocumentEnd))
      case ParseNode if !in.hasNext => getNextEvent(in, ctx.copy(state = ParseDocumentEnd))
      case ParseNode if in.peek != '-' => (MappingStart, ctx.copy(state = ParseFirstKey))
      case ParseNode if in.peek == '-' => {
        in.skip()
        if (ctx.current != ParseSequenceStart) {
          (SequenceStart, ctx.copy(state = ParseNode, current = ParseSequenceStart))
        } else {
          getNextEvent(in, ctx)
        }
      }
      case ParseFirstKey =>
        (parseScalar(in), ctx.copy(state = ParseValue))

      case ParseValue =>
        if in.peek == ':' then in.skip()
        (parseScalar(in), ctx.copy(state = ParseKey))

      case ParseKey =>
        if !in.hasCurrent || in.peek == '-' then (MappingEnd, ctx.copy(state = ParseNode))
        else if in.hasNext then (parseScalar(in), ctx.copy(state = ParseValue))
        else (MappingEnd, ctx.copy(state = ParseSequenceEnd))

      case _ => ???
    }
  }

  private def parseScalar(in: YamlReader): Scalar =
    skipWhitespace(in)
    Scalar(parseString(in))


  private def parseString(in: YamlReader): String = {
    val sb = new StringBuilder
    @tailrec
    def read(): String = {
      val c = in.peek
      val isValidChar = c != ':' && c != '\n' && c != '#'
      if isValidChar then sb.append(in.read())
      if !isValidChar || !in.hasCurrent then sb.result() else read()
    }
    read()
  }

  private def skipUntilNextToken(in: YamlReader): Unit =
    skipWhitespace(in)
    if in.peek == '#' then skipUntilNewline(in)
    skipNewline(in)


  private def skipWhitespace(in: YamlReader): Unit =
    while (in.peek == ' ')
      in.skip()

  private def skipUntilNewline(in: YamlReader): Unit =
    while (in.peek != '\n')
      in.skip()
    if in.peek == '\n' then in.skip()

  private def skipNewline(in: YamlReader): Unit =
    while (in.peek == ' ')
      in.skip()
    if in.peek == '\n' then in.skip()
