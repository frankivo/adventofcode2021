/**
 * WIP
 * https://medium.com/@nicholas.w.swift/easy-a-star-pathfinding-7e6689c7f7b2
 */

object day15 {
  def main(args: Array[String]): Unit = {
    val start = Position(0, 0)
    val end = Position(3, 3)

    astar(Node(pos = start), Node(pos = end))
  }

  type Maze = Seq[Seq[Int]]

  extension (m: Maze) {
    def height: Int = m.length
    def width: Int = m.head.length
  }

  case class Position(x: Int, y: Int) {
    def children: Set[Position] = {
      Set(
        Position(x - 1, y), // Left
        Position(x + 1, y), // Right
        Position(x, y - 1), // Top
        Position(x, y + 1), // Bottom
      ).filter(_.valid)
    }

    def valid: Boolean = x >= 0 && x < maze.width && y >= 0 && y < maze.height
  }

  case class Node(parent: Option[Node] = None, pos: Position) {
    var g = 0
    var h = 0
    var f = 0

    def children(positions: Set[Position]): Set[Node] = positions.map(Node(Some(this), _))

    override def equals(obj: Any): Boolean = {
      val other = obj.asInstanceOf[Node]
      other.pos == pos
    }
  }

  def astar(start: Node, end: Node) = {
    var open_list = Seq(start)
    var closed_list = Seq.empty[Node]
    var current_node: Node = null
    val path = Seq.empty[Position]

    while open_list.nonEmpty
    do {
      println(open_list)

      current_node = open_list.head

      // Get current node
      open_list.foreach(item =>
        if (item.f < current_node.f)
          current_node = item
      )

      open_list = open_list.filterNot(_ == current_node)
      closed_list = closed_list :+ current_node

      // Found the end?
      if (current_node.pos == end.pos) {
        println("todo!")
        println(current_node)
        System.exit(1)
      }

      val child_pos = current_node.pos.children
      val child_nodes = current_node.children(child_pos)

      child_nodes.filterNot(closed_list.contains(_))
        .foreach(c => {
          c.g = current_node.g + 1
          c.h = ((c.pos.x - end.pos.x) * 2) + ((c.pos.y - end.pos.y) * 2)
          c.f = c.g + c.h

          if (!open_list.contains(c))
            open_list = open_list :+ c
        })
    }
  }

  def maze: Maze = {
    io.Source.fromResource("day15.txt")
      .getLines()
      .map(l => l.map(_.getNumericValue))
      .toSeq
  }
}
