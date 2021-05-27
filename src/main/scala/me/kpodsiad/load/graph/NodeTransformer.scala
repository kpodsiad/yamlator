package me.kpodsiad.load.graph
import me.kpodsiad.load.parser.{YamlEvent, _}

import scala.annotation.tailrec


object NodeTransformer extends NodeTransform:


  private def convertToNodes(events: List[YamlEvent], node: Node, ctx: NodeCtx): Node = events match {
    case StreamStart :: tail => convertToNodes(tail, node, ctx.withEvents(StreamStart))
    case StreamEnd :: tail => convertToNodes(tail, node, ctx.closeBlock(StreamEnd))
    case DocumentStart :: tail => {
      val (documentEvents, restEvents) = tail.span( _ != DocumentEnd)
      val documentNode = convertToNodes(documentEvents, node, ctx)
      convertToNodes(restEvents, documentNode, ctx.withEvents(DocumentStart))
    }
    case DocumentEnd :: tail => convertToNodes(tail, node, ctx.closeBlock(DocumentEnd))
    case (SequenceStart :: tail) => {
      val (sequenceEvents, restEvents) = tail.span( _ != SequenceEnd)
      val sequenceNode = convertToNodes(sequenceEvents, NodeSequence.empty, ctx.withEvents(SequenceStart))
      convertToNodes(restEvents, node.appendChild(sequenceNode), ctx)
    }
    case SequenceEnd :: tail => convertToNodes(tail, node, ctx.closeBlock(SequenceEnd))
    case (MappingStart :: tail) => {
      val (mappingEvents, restEvents) = tail.span( _ != MappingEnd)
      val mappingNode = convertToNodes(mappingEvents, NodeMapping.empty, ctx.withEvents(MappingStart))
      convertToNodes(restEvents, node.appendChild(mappingNode), ctx.withEvents(MappingStart))
    }
    case MappingEnd :: tail => convertToNodes(tail, node, ctx.closeBlock(MappingEnd))
    case Scalar(key) :: Scalar(value) :: tail =>
      convertToNodes(tail, node.appendChild(NodeMappingElement(ScalarNode(key),ScalarNode(value))), ctx)
    case _ => node
  }

  override def fromEvents(events: List[YamlEvent]): Node = convertToNodes(events, RootNode.empty, NodeCtx.empty)
