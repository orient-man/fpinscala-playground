//package fpinscala.datastructures
object MyModule {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  // companion object
  object List {
    // allows List("a", "b") - variable parameter list
    def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }

    def tail[A](l: List[A]): List[A] = l match {
      case Cons(_, t) => t
      case Nil => throw new Exception("tail of Nil not allowed")
    }

    def setHead[A](l: List[A], h: A): List[A] = l match {
      case Cons(_, t) => Cons(h, t)
      case Nil => Cons(h, Nil)
    }

    @annotation.tailrec
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
      case (Nil, _) => Nil
      case (l, 0) => l
      case (Cons(_, t), n) => drop(t, n - 1)
    }

    @annotation.tailrec
    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case l => l
    }

    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

    def init[A](l: List[A]): List[A] = l match {
      case Cons(h, Nil) => Nil
      case Cons(h, tail) => Cons(h, init(tail))
      case Nil => throw new Exception("List too short")
    }
  }
}