package Main

import com.google.errorprone.annotations.Var

object Main {
  def main(args: Array[String]): Unit = {
    //    println("00 = " + triangle(0, 0))
    //    println("10 = " + triangle(1, 0))
    //    println("11 = " + triangle(1, 1))
    //    println("20 = " + triangle(2, 0))
    //    println("21 = " + triangle(2, 1))
    //    println("22 = " + triangle(2, 2))

    println(scales(")(dgj(slgdsl)".toList))

    println(change(4, List(2, 1)))
  }

  def triangle(c: Int, r: Int): Int = {
    if (r < 0 || r > c) 0 else if (c == 0) 1 else triangle(c - 1, r - 1) + triangle(c - 1, r)
  }

  def scales(exp: List[Char]): Boolean = {
    bracketsCounting(exp, 0)
  }

  def bracketsCounting(exp: List[Char], i: Int): Boolean = {
    if (exp.isEmpty || i < 0) return i == 0
    if (exp.head == '(') return bracketsCounting(exp.filter(e => e != exp.head), i.+(1))
    if (exp.head == ')') return bracketsCounting(exp.filter(e => e != exp.head), i.-(1))
    bracketsCounting(exp.filter(e => e != exp.head), i)
  }

  def change(amount: Int, tugriks: List[Int]): Int = {
    if (tugriks == null || tugriks.head == 0) return 0
    if (amount == 0) return 1
    if (amount < 0) return 0
    var i = 0
    for (t <- tugriks.sorted) {
      i = i + change(amount.-(t), tugriks.sorted.dropWhile(e => e < t))
    }
    i
  }
}
