package days

object Day22Prime {
  val facing = Seq(Point(1, 0), Point(0, 1), Point(-1, 0), Point(0, -1))

  case class Point(x: Int, y: Int) {
    def +(other: Point): Point = Point(x + other.x, y + other.y)

    def clockwise: Point = Point(-y, x)

    def counterClockwise: Point = Point(y, -x)
  }

  case class State(position: Point, direction: Point) {
    def password: Int = 1000 * (position.y + 1) + 4 * (position.x + 1) + facing.indexOf(direction)
  }

  def parse(input: Seq[String]): (Seq[Int], Seq[Int], Seq[Int], Seq[Int], Set[Point], Seq[(Int, String)]) = {

    val prefix = input.dropRight(2)

    val points = for {
      (row, y) <- prefix.zipWithIndex
      (col, x) <- row.zipWithIndex if col == '#'}
    yield Point(x, y)

    val maxSize = prefix.map(_.length).max
    val padded = prefix.map(_.padTo(maxSize, ' '))
    val minX = padded.map(_.segmentLength(_ == ' '))
    val maxX = padded.map(row => row.size - row.reverse.segmentLength(_ == ' '))

    val tranposed = padded.transpose
    val minY = tranposed.map(_.segmentLength(_ == ' '))
    val maxY = tranposed.map(col => col.size - col.reverse.segmentLength(_ == ' '))

    val numbers = input.last.split("\\D+").map(_.toInt).toSeq
    val letters = input.last.split("\\d+").tail.toSeq
    val moves = numbers.zipAll(letters, 0, "X")

    (minX, maxX, minY, maxY, points.toSet, moves)
  }


  def part2(input: Seq[String]): Int = {

    val (minX, _, _, _, points, moves) = parse(input)
    val initial = State(Point(minX.head, 0), Point(1, 0))

    val result = moves.foldLeft(initial) { case (state, (number, letter)) =>
      val nextState = (1 to number).foldLeft(state) { case (state, _) =>
        val State(position, direction) = state
        val cubeX = position.x / 50
        val cubeY = position.y / 50
        val modX = position.x % 50
        val modY = position.y % 50
        val next = position + direction
        val nextX = if (next.x >= 0) next.x / 50 else next.x
        val nextY = if (next.y >= 0) next.y / 50 else next.y

        val (candidatePosition, candidateDirection) =
          if (cubeX == nextX && cubeY == nextY)
            (next, direction)
          else {
            // Cube sides:
            //  AB
            //  C
            // ED
            // F
            (cubeX, cubeY, nextX, nextY) match {
              // A to F
              case (1, 0, 1, -1) => Point(0, 150 + modX) -> Point(1, 0)
              // A to C
              case (1, 0, 1, 1) => next -> direction
              // A to E
              case (1, 0, 0, 0) => Point(0, 149 - modY) -> Point(1, 0)
              // A to B
              case (1, 0, 2, 0) => next -> direction

              // B to F
              case (2, 0, 2, -1) => Point(0 + modX, 199) -> direction
              // B to C
              case (2, 0, 2, 1) => Point(99, 50 + modX) -> Point(-1, 0)
              // B to A
              case (2, 0, 1, 0) => next -> direction
              // B to D
              case (2, 0, 3, 0) => Point(99, 149 - modY) -> Point(-1, 0)

              // C to A
              case (1, 1, 1, 0) => next -> direction
              // C to D
              case (1, 1, 1, 2) => next -> direction
              // C to E
              case (1, 1, 0, 1) => Point(modY, 100) -> Point(0, 1)
              // C to B
              case (1, 1, 2, 1) => Point(100 + modY, 49) -> Point(0, -1)

              // D to C
              case (1, 2, 1, 1) => next -> direction
              // D to F
              case (1, 2, 1, 3) => Point(49, 150 + modX) -> Point(-1, 0)
              // D to E
              case (1, 2, 0, 2) => next -> direction
              // D to B
              case (1, 2, 2, 2) => Point(149, 49 - modY) -> Point(-1, 0)

              // E to C
              case (0, 2, 0, 1) => Point(50, 50 + modX) -> Point(1, 0)
              // E to F
              case (0, 2, 0, 3) => next -> direction
              // E to A
              case (0, 2, -1, 2) => Point(50, 49 - modY) -> Point(1, 0)
              // E to D
              case (0, 2, 1, 2) => next -> direction

              // F to E
              case (0, 3, 0, 2) => next -> direction
              // F to B
              case (0, 3, 0, 4) => Point(100 + modX, 0) -> direction
              // F to A
              case (0, 3, -1, 3) => Point(50 + modY, 0) -> Point(0, 1)
              // F to D
              case (0, 3, 1, 3) => Point(50 + modY, 149) -> Point(0, -1)
            }
          }

        if (points.contains(candidatePosition)) state else State(candidatePosition, candidateDirection)
      }


      val nextDirection = letter match {
        case "L" => nextState.direction.counterClockwise
        case "R" => nextState.direction.clockwise
        case _ => nextState.direction
      }
      State(nextState.position, nextDirection)
    }
    result.password
  }

  def main(args: Array[String]): Unit = {
    val data = io.Source.fromResource("day22.txt").getLines().toSeq
    println(part2(data))
  }
}