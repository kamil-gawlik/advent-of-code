def rec(num1: Long): String = {
  def rec2(acc: String, num: Long): String = {
    (num / 5, num % 5) match {
      case (l, r) if l == 0 && r == 0 => acc
      case (l, r) if r > 0 && r < 3 =>
        rec2(acc + r, l)
      case (l, r) if r == 3 =>
        rec2(acc + "=",
          ((num + 2) / 5)
        )
      case (l, r) if r == 4 =>
        rec2(acc + "-",
          ((num + 1) / 5)
        )
      case (l, r) => rec2(acc + r, l)
    }
  }

  rec2("", num1).reverse
}


println(rec(1))
println(rec(2))
println(rec(3))
println(rec(4))
println(rec(5))
println(rec(6))
println(rec(7))
println(rec(8))
println(rec(9))
println(rec(10))
println(rec(15))
println(rec(20))
println(rec(2022))
println(rec(12345))
println(rec(314159265))


