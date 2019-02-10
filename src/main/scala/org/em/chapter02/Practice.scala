package org.em.chapter02

object Practice extends App {

  def fib(n: Int): Int = {
    if (n == 0) return 0

    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)

    loop(n - 1, 0, 1)
  }

  for (n <- 1 to 6) {
    println(fib(n))
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) go(n + 1)
      else false

    go(0)
  }

  def ordered(a: Int, b: Int): Boolean = {
    a < b
  }

  println(isSorted(Array(5, 4, 3, 2, 1), ordered))

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
