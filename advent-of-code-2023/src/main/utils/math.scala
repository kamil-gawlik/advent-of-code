package utils

trait math {

  def gcd(x: Long, y: Long): Long =
    if (y == 0)
      x
    else
      gcd(y, x % y)

  def lcm(numbers: Seq[Long], index: Int = 0): Long = {
    if (index == numbers.length - 1) {
      return numbers(index)
    }
    val a = numbers(index)
    val b = lcm(numbers, index + 1)
    (a * b) / gcd(a, b)
  }

  def lcm(n1: Long, n2: Long): Long = lcm(Seq(n1, n2))
}
