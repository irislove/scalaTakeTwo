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
  
  test("map") {
    new TestSets {
      val s4 = singletonSet(4)
      val s5 = singletonSet(5)
      val s6 = singletonSet(6)
      val s7 = singletonSet(1000)
      
     val s = union(s1, union(s2, union(s3, union(s4, union(s5, union(s6, s7))))))
     val s8 = map(s, (x: Int) => x - 1)
     assert(contains(s8, 0), "0")
     assert(contains(s8, 1), "1")
     assert(contains(s8, 2), "2")
     assert(contains(s8, 3), "3")
     assert(contains(s8, 4), "4")
     assert(contains(s8, 5), "5")
     assert(contains(s8, 999), "999")
    }
  }
  
  test("forall and map") {
    new TestSets {
      val s4 = singletonSet(4)
      val s5 = singletonSet(5)
      val s6 = singletonSet(6)
      
     val s = union(s1, union(s2, union(s3, union(s4, union(s5, s6)))))
     val s8 = map(s, (x: Int) => x * 2)
     assert(contains(s8, 2), "2")
     assert(contains(s8, 4), "4")
     assert(contains(s8, 6), "6")
     assert(contains(s8, 8), "8")
     assert(contains(s8, 10), "10")
     assert(contains(s8, 12), "12")
     assert(!contains(s8, 2000), "2000 is not in the set")
     assert(forall(s8, (x: Int) => x % 2 == 0))
    }
  }
  
  test("forall within bound") {
    new TestSets {
      
     val s = union(s1, union(s2, s3))
     
     assert(forall(s, (x: Int) => x < 4), "all elements are < 4")
    }
  }
  
  test("forall out of bound") {
    new TestSets {
     val s4 = singletonSet(1001)
      
     val s = union(s1, union(s2, union(s3, s4)))
     
     assert(forall(s, (x: Int) => x < 4), "not all elements are < 4 but 1001 is > bound")
    }
  }
  
  test("forall false") {
    new TestSets {
     val s4 = singletonSet(1000)
      
     val s = union(s1, union(s2, union(s3, s4)))
     
     assert(!forall(s, (x: Int) => x < 3), "not all elements are < 3")
    }
  }
  
  test("exists true") {
    new TestSets {
     val s4 = singletonSet(1000)
      
     val s = union(s1, union(s2, union(s3, s4)))
     
     assert(exists(s, (x: Int) => x < 3), "one or more elements are < 3")
    }
  }
  
  test("exists false") {
    new TestSets {
     val s4 = singletonSet(1000)
      
     val s = union(s1, union(s2, union(s3, s4)))
     
     assert(!exists(s, (x: Int) => x > 1000), "no elements is > 1000")
    }
  }
  
  test("exists given {1, 2, 3, 4, 1000}") {
    new TestSets {
     val s4 = singletonSet(4)
     val s5 = singletonSet(1000)
      
     val s = union(s1, union(s2, union(s3, union(s4, s5))))
     
     assert(!exists(s, (x: Int) => x == 5), "no elements is 5")
    }
  }
  
  test("exists and filter") {
    new TestSets {
     val s4 = singletonSet(4)
     val s5 = singletonSet(1000)
      
     val s = filter(union(s1, union(s2, union(s3, union(s4, s5)))), (x: Int) => x % 2 == 1)
     
     assert(exists(s, (x: Int) => x % 2 == 1), "some elements are odd")
    }
  }
  
  test("forall empty set") {
    new TestSets {
     val s4 = (x: Int) => false
     
     assert(!forall(s4, (x: Int) => x < 4), "empty set")
    }
  }
}
