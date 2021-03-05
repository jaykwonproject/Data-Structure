/**
 * cse250.pa0.tests.FunctionTests.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT:hyukjook
 * Person#:50184139
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa0.tests

import cse250.pa0.objects.Functions
import org.scalatest.FlatSpec

class FunctionTests extends FlatSpec {
  // Tests for problem 1.
  behavior of "FunctionsTest.genNum"

  it should "Generate positive numbers ranging from 0 to n" in {
    for (n <- 0 to 1000) { //using values that sufficiently test genNum.
      val num = Functions.genNum(n)
      assert(num >= 0 && num <= n) //return True if the returned value is greater or equal to 0, else false
    }
  }


  // Tests for problem 2.
  behavior of "FunctionsTest.genSeq"

  it should "Generate a positive sequence" in {
    for (n <- 0 to 1000) { //the range of n is from 0 to 1000
      val seq = Functions.genSeq(n) // seq is the generated sequence
      for (i <- seq) {
        assert(seq(i) > 0) //Every item in the returned sequence should be positive
      }
      assert(seq.length == n) //the length of the returned sequence should be n
    }
  }


  // Tests for problem 3.
  behavior of "FunctionsTest.funThree"

  it should "test if it is a bitonic sequence" in {
    assert(Functions.funThree(1)<Functions.funThree(2)) //make sure it starts with an increasing sequence
    for(n<- 1 to 10000){ //n is any positive number up to 10,000
      val currentEl = Functions.funThree(n)
      if(n!=10000){
        val nextEl = Functions.funThree(n+1)
        if(currentEl > nextEl){  //if it's starting to decrease -> make sure it's keep decreasing till the end
          for(i<- n until 10000){
            val cEl = Functions.funThree(i)
            val nEl = Functions.funThree(i+1)
            assert(cEl>nEl) // check if it's keep decreasing
          }
        }
        else{ //if it's keep increasing until the last two sequence, then it's just an increasing sequence.
          assert(n!=9999) //if 9999th element is still smaller than 10000th element, it's not a bitonic sequence.
        }
      }
    }
  }


  // Tests for problem 4.
  behavior of "FunctionsTest.mapSum"
  // 3 cases where n would be 0, and 1 and greater than 1
  def f1(x: Int) = 0
  def f2(x: Int) = 1
  def f3(x: Int) = x*x

  it should "return 0 when n is 0" in { //because 0 times i = 0 always
    for(i<-0 to 1000){
      assert(Functions.mapSum(i,f1)==0)
    }
  }
  it should "return n when n is 1" in { //because 1 times i = i always
    for(i<-0 to 1000){
      assert(Functions.mapSum(i,f2)==i)
    }
  }
  it should "return the sum of squared numbers" in {
    for(i<-0 to 1000){
      val sum = Functions.mapSum(i, f3)
      val res = (i*(i+1)*((2*i)+1))/6 //sum of squares
      assert(sum==res)
    }
  }
}

