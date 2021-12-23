/**
 * WIP
 * https://medium.com/@nicholas.w.swift/easy-a-star-pathfinding-7e6689c7f7b2
 */

object day15 {
  def main(args: Array[String]): Unit = {
    val start = Position(0, 0)
    val end = Position(2, 2)

    astar(Node(start), Node(end))
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

  case class Node(pos: Position, parent: Node = null) {
    var g = 0
    var h = 0
    var f = 0

    def children: Set[Node] = pos.children.map(Node(_, this))

    override def equals(obj: Any): Boolean = {
      val other = obj.asInstanceOf[Node]
      other.pos == pos
    }
  }

  def astar(start: Node, end: Node) = {
    var open_list = Seq(start)
    var closed_list = Seq.empty[Node]
    var current_node = open_list.head

    while open_list.nonEmpty
    do {
      current_node = open_list.head

      // Get current node
      open_list.foreach(item =>
        if (item.f < current_node.f)
          current_node = item
      )

      open_list = open_list.filterNot(_.pos == current_node.pos)
      closed_list = closed_list :+ current_node

      current_node
        .children
        .filterNot(closed_list.contains(_))
        .foreach(c => {
          c.g = current_node.g + 1
          c.h = math.pow(c.pos.x - end.pos.x, 2).toInt + math.pow(c.pos.y - end.pos.y, 2).toInt
          c.f = c.g + c.h

          if (!open_list.contains(c))
            if (!open_list.exists(c.g > _.g))
              open_list = open_list :+ c
          //              println(open_list)
          //              println(closed_list)
        })
    }

    println("huh" + current_node.pos)
    println("huh2" + current_node.parent.pos)


    var n = current_node
    var path = Set.empty[Position]
    while n.parent != null
      do {
      path += n.pos
      n = n.parent
    }

    println(path)
//
//    current_node
//      .parents
//      .map(_.pos)
//      .foreach(println)
//
//    println(current_node.parents.size)
//    println(maze.length * maze.height)
  }

  def maze: Maze = {
    io.Source.fromResource("day15.txt")
      .getLines()
      .map(l => l.map(_.getNumericValue))
      .toSeq
  }
}
