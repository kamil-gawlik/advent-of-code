package days

import utils._

object Day20 {

  def main(args: Array[String]): Unit = {
    part2()
  }

  private def part1(): Unit = {
    val nodes = createNodes(readNumbers())
    mix(nodes)

    def skip(n: Node) = Iterator.iterate(n)(_.next).drop(1000).next()

    val startNode = nodes.find(_.value == 0).get
    println(Iterator.iterate(startNode)(skip)
      .drop(1).take(3).map(_.value).sum)
  }

  private def mix(nodes: Seq[Node]) = {
    for (n <- nodes) {
      val remainder = (n.value % (nodes.size - 1)).toInt
      val move = if (remainder >= 0) remainder else remainder + nodes.size - 1
      for (_ <- 1 to move) {
        n.moveRight()
      }
    }
  }

  private def part2(): Unit = {
    val nodes = createNodes(readNumbers(), 811589153L)
    for (_ <- 0 until 10) {
      mix(nodes)
    }

    def iterator(n: Node) = Iterator.iterate(n)(_.next).take(nodes.size)

    def skip(n: Node) = Iterator.iterate(n)(_.next).drop(1000).next()

    val startNode = nodes.find(_.value == 0).get

    println(Iterator.iterate(startNode)(skip)
      .drop(1).take(3).map(_.value).sum)
  }

  def createNodes(s: Seq[Long], key: Long = 1L): Seq[Node] = {
    val nodes = s.map(int => Node(int * key, null, null))
    nodes.zipWithIndex.foreach { case (node, idx) =>
      node.prev = nodes((idx - 1 + nodes.size) % nodes.size)
      node.next = nodes((idx + 1) % nodes.size)
    }
    nodes
  }

  private def readNumbers(): Seq[Long] = {
    readLines("day20.txt")
      .map(_.toLong)
  }

  case class Node(value: Long, var prev: Node, var next: Node) {
    override def toString: String = value.toString

    def moveRight(): Unit = {
      val (prevNode, thisNode, nextNode, nextNextNode) = (prev, this, next, next.next)
      prevNode.next = nextNode
      nextNode.prev = prevNode
      nextNode.next = thisNode
      thisNode.prev = nextNode
      thisNode.next = nextNextNode
      nextNextNode.prev = thisNode
    }

  }

}
