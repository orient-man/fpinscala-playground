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
      case _ => l
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

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    @annotation.tailrec
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

    def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

    def length[A](as: List[A]): Int = foldRight(as, 0)((_, n) => n + 1)

    def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil:List[A])((xs, x) => Cons(x, xs))

    def append2[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((xs, x) => Cons(x, xs))

    def append3[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

    // non linear version
    def collect[A](as: List[List[A]]): List[A] = foldLeft(as, Nil:List[A])((a1, a2) => append2(a1, a2))

    // non tail recursive
    def collect2[A](as: List[List[A]]): List[A] = foldRight(as, Nil:List[A])(append2)

    def collect3[A](as: List[List[A]]): List[A] = foldLeft(reverse(as), Nil:List[A])((acc, as) => append2(as, acc))

    def add1(ns: List[Int]) = reverse(foldLeft(ns, Nil:List[Int])((xs, x) => Cons(x + 1, xs)))

    def map[A,B](as: List[A])(f: A => B): List[B] = reverse(foldLeft(as, Nil:List[B])((xs, x) => Cons(f(x), xs)))

    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldLeft(reverse(as), Nil:List[A])((xs, x) => if (f(x)) Cons(x, xs) else xs)

    // e.g. List.flatMap(List(1,2,3))(i => List(i,i))
    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
      foldLeft(reverse(as), Nil:List[B])((xs, x) => append2(f(x), xs))

    def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if (f(x)) Cons(x, Nil) else Nil)

    def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
    }

    @annotation.tailrec
    def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
      case _ => false
    }

    @annotation.tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
      case Nil => false
      case _ if (startsWith(sup, sub)) => true
      case Cons(_, tail) => hasSubsequence(tail, sub)
    }
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(left, right) => size(left) + size(right) + 1
    }

    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(x) => x
      case Branch(left, right) => maximum(left).max(maximum(right))
    }

    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 0
      case Branch(left, right) => depth(left).max(depth(right)) + 1
    }

    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

    def fold[A,B](t: Tree[A], z: B)(f: (A, B) => B): B = t match {
      case Leaf(a) => f(a, z)
      case Branch(left, right) => fold(right, fold(left, z)(f))(f)
    }

    def size2[A](t: Tree[A]): Int = fold(t, 0)((_, n) => n + 1)
  }
}