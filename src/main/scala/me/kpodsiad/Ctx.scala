package me.kpodsiad

case class Ctx(
  line: Int,
  column: Int,
  indents: List[Int] = List.empty,
  anchors: Map[String, Any] = Map.empty
) {
  def nextLine = this.copy(line = this.line + 1, column = 0)
  def atLine(line: Int) = this.copy(line = line)
  def atColumn(column: Int) = this.copy(column = column)
  def nextColumn = this.copy(column = this.column + 1)
}

object Ctx:
  final val EmptyCtx = Ctx(0, 0)