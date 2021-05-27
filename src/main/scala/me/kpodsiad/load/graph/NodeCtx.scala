package me.kpodsiad.load.graph

import me.kpodsiad.load.parser.YamlEvent

import java.lang.{Runtime, RuntimeException}

case class NodeCtx(previousStates: List[YamlEvent])  {
  def withEvents(event: YamlEvent): NodeCtx = NodeCtx(event :: previousStates)
  
  def closeBlock(event: YamlEvent): NodeCtx = previousStates match {
    case event :: tail => NodeCtx(tail)
    case _ => throw new RuntimeException("Unexpected close blocks")
  }
}

case object NodeCtx {
  def empty: NodeCtx = NodeCtx(Nil)
}