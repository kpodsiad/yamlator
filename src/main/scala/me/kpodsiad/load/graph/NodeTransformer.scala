package me.kpodsiad.load.graph
import me.kpodsiad.load.parser.YamlEvent
import me.kpodsiad.load.parser._

import scala.annotation.tailrec


object NodeTransformer extends NodeTransform:


  private def convertMappingToNode(events: List[YamlEvent], node: NodeMapping): NodeMapping = events match {
    case Scalar(key) :: Scalar(value) :: tail =>  convertMappingToNode(tail, node.appendChild(key,value))
    case _ | Nil => node // TODO - catch unexpected event
  }

  private def convertSequenceToNode(events: List[YamlEvent], node: NodeSequence): NodeSequence = events match  {
    case (MappingStart :: tail) => {
      val (mappingEvents, restEvents) = tail.span( _ != MappingEnd)
      val mappingNode = convertMappingToNode(mappingEvents, NodeMapping.empty)
      convertSequenceToNode(restEvents, node.appendChild(mappingNode))
    }
    case MappingEnd :: tail => convertSequenceToNode(tail, node)
    case Nil => node
  }

  private def convertDocumentToNode(events: List[YamlEvent]): List[Node] = events match  {
    case (SequenceStart :: tail) => {
      val (sequenceEvents, restEvents) = tail.span( _ != SequenceEnd)
      val sequenceNode = convertSequenceToNode(sequenceEvents, NodeSequence.empty)
      sequenceNode :: convertDocumentToNode(restEvents)
    }
    case SequenceEnd :: tail => convertDocumentToNode(tail)
    case Nil => Nil
  }
//
//  private def convertToNodesAbstract(events: List[YamlEvent], start: YamlEvent, end: YamlEvent, node: Node): events match {
//      case start :: tail => convertToNodesAbstract(tail, )
//  }


  private def convertToNodes(events: List[YamlEvent], node: RootNode): Node = events match {
    case StreamStart :: tail => convertToNodes(tail, node) // TODO - add representation in node structure
    case DocumentStart :: tail => {
      val (documentEvents, restEvents) = tail.span( _ != DocumentEnd)
      val documentNode = convertDocumentToNode(documentEvents)
      convertToNodes(restEvents, node.appendAllChild(documentNode))
    }
    case DocumentEnd :: tail => convertToNodes(tail, node)
    case StreamEnd :: tail => convertToNodes(tail, node) // TODO - add representation in node structure
    case Nil => node
  }

  override def fromEvents(events: List[YamlEvent]): Node = convertToNodes(events, RootNode.empty)
