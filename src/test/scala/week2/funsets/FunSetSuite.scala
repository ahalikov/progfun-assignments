package week2.funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val s4: Set = x => Array(2, 6, 9, 16) contains(x)
    val s5: Set = x => Array(2, 4, 8, 16, 28, 50) contains(x)

    val mod2: Set = (x: Int) => x % 2 == 0
    val mod3: Set = (x: Int) => x % 3 == 0
    val mod5: Set = (x: Int) => x % 5 == 0
  }

  test("singletonSet(1) contains 1") {
    new TestSets {
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("that union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("that filter returns right subset") {
    new TestSets {
      val filtered = filter(s4, mod3)
      assert(filtered(9), "Filtered set contains 9")
      assert(!filtered(2), "Filtered set does not contain 2")
      assert(!filtered(4), "Filtered set does not contain 4")
      assert(!filtered(8), "Filtered set does not contain 8")
      assert(!filtered(16), "Filtered set does not contain 16")
    }
  }

  test("that forall works well") {
    new TestSets {
      assert(!forall(s4, mod2), "9 in s4 does not divide by 2")
      assert(forall(s5, mod2), "all items in s5 divide by 2")
    }
  }

  test("that exists works well") {
    new TestSets {
      assert(exists(s4, mod3))
      assert(exists(s4, mod3))
    }
  }

  test("that that map applies div2 to set") {
    new TestSets {
      val div2: Int => Int = (x: Int) => x / 2
      val newSet = map(s4, div2)
      assert(newSet(1))
      assert(newSet(3))
      assert(newSet(4))
      assert(newSet(8))

      assert(!newSet(2))
      assert(!newSet(6))
      assert(!newSet(9))
      assert(!newSet(16))
    }
  }
}
