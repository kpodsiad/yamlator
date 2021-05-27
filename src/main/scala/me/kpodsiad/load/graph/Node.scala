package me.kpodsiad.load.graph

sealed trait Node

trait ScalarNode[T] extends Node
trait Sequence extends Node {
  def appendChild(value: Node): Sequence
}

case object EmptyNode extends Node

// scalar
case class ScalarNodeInt(value: Int) extends ScalarNode[Int]
case class ScalarNodeString(value: String) extends ScalarNode[String]

// sequence
case class NodeSequence(sequence: List[Node]) extends Sequence {
  def appendChild(value: Node): NodeSequence = {
    copy(sequence :+ value)
  }
}

case object NodeSequence {
  def empty: NodeSequence = NodeSequence(Nil)
}

// mapping
case class NodeMapping(map: Map[String,Any]) extends Node {
  def appendChild(key: String, value: Any): NodeMapping = {
    copy(map = map + (key-> value))
  }
}

case object NodeMapping {
  def empty: NodeMapping = NodeMapping(Map.empty)
}

// root
case class RootNode(nodes: List[Node]) extends Node {
  def appendChild(node: Node) = {
    RootNode(nodes :+ node)
  }

  def appendAllChild(appendedNodes: List[Node]) = {
    RootNode(nodes ++ appendedNodes)
  }
}

case object RootNode {
  def empty: RootNode = RootNode(Nil)
}