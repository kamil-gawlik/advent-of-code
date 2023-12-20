package utils

trait check {

  def check[T](f: () => T, expected: T, marker: String = ""): Unit = timeMs(marker) {
    val result = f()
    if (result == expected) {
      println(
        s"""${Console.GREEN} passed.
           |  got: [$result] ${Console.RESET}""".stripMargin)
    } else println(
      s"""${Console.RED} failed!
         |  got:        [$result]
         |  expected:    [$expected] ${Console.RESET}""".stripMargin)
  }

}
