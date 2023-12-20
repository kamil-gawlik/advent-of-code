package utils

trait time {

  def timeMs[R](marker: String)(block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()
    val header = if (marker != "") s"[$marker] " else ""
    println(s"$header elapsed time: " + (t1 - t0) + "ms")
    result
  }
}
