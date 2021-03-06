package funsets

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
    val negative = (x: Int) => x < 0
    val positive = (x: Int) => x > 0
    val even = (x: Int) => x % 2 == 0
    val odd = (x: Int) => x % 2 != 0
    val kelvin = (x: Int) => x >= -273
    val rating5 = (x: Int) => x >= 0 && x <= 5
    val rating10 = (x: Int) => x >= 0 && x <= 10
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2), "2 is not 1")
      assert(!contains(s1, 0), "0 is not 1")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains only elements containing in both sets") {
    new TestSets {
      val s = intersect(negative, kelvin)
      assert(contains(s, -273), "has -273")
      assert(!contains(s, 273), "hasn't 273")
      assert(!contains(s, 2), "hasn't 2")
      assert(!contains(s, 0), "hasn't 0")
      assert(contains(s, -1), "has -1")
      assert(contains(s, -100), "has -100")
    }
  }

  test("diff contains only elements containing in first set and not in second set") {
    new TestSets {
      val s = diff(rating10, rating5)
      assert(!contains(s, 11))
      assert(contains(s, 10))
      assert(contains(s, 9))
      assert(contains(s, 8))
      assert(contains(s, 7))
      assert(contains(s, 6))
      assert(!contains(s, 5))
      assert(!contains(s, 4))
      assert(!contains(s, 3))
      assert(!contains(s, 0))
      assert(!contains(s, -1))
    }
  }

  test("filter contains only elements containing in set and not hold condition from predicate") {
    new TestSets {
      val s = filter(positive, (x: Int) => x > 7 && x < 10)
      assert(!contains(s, -1))
      assert(!contains(s, 10))
      assert(contains(s, 9))
      assert(contains(s, 8))
      assert(!contains(s, 7))
      assert(!contains(s, 6))
      assert(!contains(s, 5))
      assert(!contains(s, 4))
      assert(!contains(s, 3))
      assert(!contains(s, 0))
      assert(!contains(s, -1))
    }
  }

  test("forall works as expected") {
    new TestSets {
      assert(!forall(even, (x: Int) => x > -10 && x < 10))
      assert(!forall(odd, (x: Int) => x > -10 && x < 10))
      assert(!forall(rating10, (x: Int) => x > 5 && x < 10))
      assert(forall(rating10, (x: Int) => x > -5 && x < 100))
      assert(forall(negative, (x: Int) => x < 0))
      assert(forall(positive, (x: Int) => x >= 0))
      assert(!forall(positive, (x: Int) => x > 1))
    }
  }

  test("exists works as expected") {
    new TestSets {
      assert(exists(even, x => x == -10))
      assert(!exists(odd, x => x == -10))
      assert(exists(even, x => x == 1000))
      assert(exists(even, x => x == -1000))
      assert(exists(odd, x => x == 999))
      assert(exists(odd, x => x == -999))
      assert(exists(even, x => x > -10 && x < 10))
      assert(exists(odd, x => x > -10 && x < 10))
      assert(exists(rating10, x => x > 5 && x < 10))
      assert(exists(rating10, x => x > -5 && x < 100))
      assert(exists(negative, x => x < 0))
      assert(exists(positive, x => x >= 0))
      assert(!exists(positive, x => x < 0))
      assert(exists(positive, x => x > 1))
    }
  }

  test("map works as expected") {
    val _123 = (x: Int) => x >= 1 && x <= 3

    new TestSets {
      assert(funsets.FunSets.toString(map(_123, x => x)) === "{1,2,3}")
      assert(funsets.FunSets.toString(map(_123, x => -x)) === "{-3,-2,-1}")
      assert(funsets.FunSets.toString(map(_123, x => x-x)) === "{0}")
      assert(funsets.FunSets.toString(map(_123, x => x+x)) === "{2,4,6}")
      assert(funsets.FunSets.toString(map(_123, x => x*x)) === "{1,4,9}")
      assert(funsets.FunSets.toString(map(_123, x => x-1)) === "{0,1,2}")
    }
  }


}
