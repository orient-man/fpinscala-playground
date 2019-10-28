object MyModule {
  def abs(n: Int): Int = if (n < 0) -n else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc else go(n-1, n*acc)

    go(n, 1)
  }

  // Exercise 2.1: tail recursive fibonacci
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc1: Int, acc2: Int): Int =
      if (n <= 0) acc1
      else if (n == 1) acc2
      else go(n-1, acc2, acc1 + acc2)

    go(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
        if (n >= as.length - 1) true
        else if (!ordered(as(n), as(n + 1))) false
        else loop(n + 1)

    loop(0)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit =
    println(formatResult("absolute value", -42, fib))
}