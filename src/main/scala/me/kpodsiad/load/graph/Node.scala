package me.kpodsiad.load.graph

sealed trait Node {
  def appendChild(value: Node): Node
}

trait Sequence extends Node {
}

case class EmptyNode() extends Node  {
  def appendChild(value: Node): Node = value
}

case object EmptyNode extends Node {
  def appendChild(value: Node): Node = value
}

// scalar
case class ScalarNode(value: Any) extends Node {
  def appendChild(value: Node) = sys.error("Forbidden operation")
}

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
case class NodeMapping(map: List[NodeMappingElement]) extends Node {

  def appendChild(node: Node): Node = node match {
    case  element: NodeMappingElement => NodeMapping(map :+element)
    case _ => sys.error("Forbidden operation")
  }
}

case class NodeMappingElement(key: ScalarNode, value: ScalarNode) extends Node {
  def appendChild(node: Node): Node = ???
}

case object NodeMapping {
  def empty: NodeMapping = NodeMapping(Nil.empty)
}

// root
case class RootNode(nodes: List[Node]) extends Node {
  def appendChild(node: Node): RootNode = {
    RootNode(nodes :+ node)
  }

  def appendAllChild(appendedNodes: List[Node]) = {
    RootNode(nodes ++ appendedNodes)
  }
}

case object RootNode {
  def empty: RootNode = RootNode(Nil)
}