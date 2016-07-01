/***
 * Lecture 2.2: Currying
 */
def sum(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumF(a + 1, b)

  sumF
}

sum(x => x)(1, 3)

def fact(n: Int): Int =
  if (n < 2) 1
  else
    n * fact(n - 1)

assert(fact(0)  == 1)
assert(fact(1)  == 1)
assert(fact(2)  == 2)
assert(fact(3)  == 6)
assert(fact(4)  == 24)
assert(fact(7)  == 5040)
assert(fact(10) == 3628800)

def sumInts       = sum(x => x)
def sumCubes      = sum(x => x * x * x)
def sumFactorials = sum(fact)

assert(sumFactorials(1, 4) == fact(1) + fact(2) + fact(3) + fact(4))

// Multiple parameter list
// Shorter than previous version with sumF

def sum2(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum2(f)(a + 1, b)

sum2(x => x) (1, 3)

// Generalization of sum and product as mapReduce
object exercise {
  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)

  def fact(n: Int) = product(x => x)(1, n)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

  def productMapReduce(f: Int => Int)(a: Int, b: Int) = mapReduce(f, (x, y) => x * y, 1)(a, b)

  def test(): Unit = {
    product(x => x)(1, 5) == fact(5)
  }
}

exercise.test()
