package org.em

import scala.collection.immutable.List

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {a <- this; b1 <- b} yield f(a, b1)
}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def traverse[E,A,B](es: scala.collection.immutable.List[A])(f: A => Either[E, B]): Either[E, scala.collection.immutable.List[B]] =
    es match {
      case scala.collection.immutable.Nil => Right(scala.collection.immutable.Nil)
      case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
    }

  def traverse_1[E,A,B](es: scala.collection.immutable.List[A])(f: A => Either[E, B]): Either[E, scala.collection.immutable.List[B]] =
    es.foldRight[Either[E,scala.collection.immutable.List[B]]](Right(scala.collection.immutable.Nil))((a, b) => f(a).map2(b)(_ :: _))

  def sequence[E,A](es: scala.collection.immutable.List[Either[E,A]]): Either[E,scala.collection.immutable.List[A]] =
    traverse(es)(x => x)
}
