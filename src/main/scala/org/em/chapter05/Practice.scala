package org.em.chapter05


object Practice extends App {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  Stream(1, 2, 3, 4, 5, 6).toList

  println(Stream(1, 2, 3, 4, 5, 6).take(3).toList)

  println(Stream(1, 2, 3, 4, 5, 6).exists(a => a == 3))

}
