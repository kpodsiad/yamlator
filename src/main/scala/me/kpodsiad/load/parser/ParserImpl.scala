package me.kpodsiad.load.parser

import me.kpodsiad.YamlReader

object ParserImpl extends Parser :
  override def getNextEvent(in: YamlReader, ctx: ParserCtx): (YamlEvent, ParserCtx) = {
    if in.hasNext then skipUntilNextToken(in)
    ctx.state match {
      case EndState => (YamlEventEnd, ctx)
      case ParseStreamStart => (StreamStart, ctx.copy(state = ParseDocumentStart))
      case ParseStreamEnd => (StreamEnd, ctx.copy(state = EndState))
      case ParseDocumentStart => (DocumentStart, ctx.copy(state = ParseNode))
      case ParseDocumentEnd => (DocumentEnd, ctx.copy(state = ParseStreamEnd))
      case ParseNode if in.peek != '-' => (MappingStart, ctx.copy(state = ParseFirstKey))
      case ParseFirstKey =>
        (parseScalar(in), ctx.copy(state = ParseValue))

      case ParseValue =>
        if in.peek == ':' then in.skip()
        (parseScalar(in), ctx.copy(state = ParseKey))

      case ParseKey =>
        if in.hasNext then (parseScalar(in), ctx.copy(state = ParseValue))
        else getNextEvent(in, ctx.copy(state = ParseDocumentEnd))

      case _ => ???
    }
  }

  private def parseScalar(in: YamlReader): Scalar =
    skipWhitespace(in)
    Scalar(parseString(in))


  private def parseString(in: YamlReader): String = {
    val sb = new StringBuilder
    while ( {
      val c = in.peek
      c != ':' && c != '\n' && c != '#'
    }) {
      sb.append(in.read())
    }
    sb.result()
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
