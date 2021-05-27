package me.kpodsiad.load.graph

import me.kpodsiad.load.parser.YamlEvent

trait NodeTransform {
  
  def fromEvents(events: List[YamlEvent]): Node

}
