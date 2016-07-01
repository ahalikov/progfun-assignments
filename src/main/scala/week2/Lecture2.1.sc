/**
 * Lecture 2.1: Introduction to high-order functions
 */

def sum(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum(f, a + 1, b)

def sumInts(a: Int, b: Int) = sum(x => x, a, b)

def sumCubes(a: Int, b: Int) = sum(x => x * x * x, a, b)


sum(x => x, 1, 3)
sumInts(1, 3)
sumCubes(1, 3)

def sumTailRec(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  }
  loop(a, 0)
}

sumTailRec(x => x)(1, 3)
