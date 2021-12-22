/**
 * WIP
 * https://medium.com/@nicholas.w.swift/easy-a-star-pathfinding-7e6689c7f7b2
 */

object day15 {
  def main(args: Array[String]): Unit = {
    val start = Position(0, 0)
    val end = Position(maze.width, maze.height)

    val n = Node(pos = Position(0, 0))
    n.g = 2
  }

  type Maze = Seq[Seq[Int]]

  extension (m: Maze) {
    def height: Int = m.length
    def width: Int = m.head.length
  }

  case class Position(x: Int, y: Int)

  case class Node(parent: Option[Node] = None, pos: Position) {
    var g = 0
    var h = 0
    var f = 0
  }

  def astar(start: Position, end: Position) = {
    val node_start = Node(pos = start)
    val node_end = Node(pos = end)

    val open_list = Seq(node_start)
    val closed_list = Seq.empty[Node]
    val current_node: Node = _
    val path = Seq.empty[Position]

    while open_list.nonEmpty
    do {
      val current_node = open_list.head

      // Get current node
      open_list.foreach(item =>
        if (item.f < current_node.f)
          current_node = item
      )

      open_list = open_list - current_node
      closed_list = closed_list + current_node

      // Found the end?
      if (current_node == node_end) {
        println("todo!")
      }
    }
  }

  def maze: Maze = {
    io.Source.fromResource("day15.txt")
      .getLines()
      .map(l => l.map(_.getNumericValue))
      .toSeq
  }
}
