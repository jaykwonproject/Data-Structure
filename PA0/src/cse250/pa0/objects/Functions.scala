/**
 * cse250.pa0.objects.Functions.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */
package cse250.pa0.objects

object Functions {
  def genNum(n: Int): Int = {
    0
  }

  def genSeq(n: Int): Seq[Int] = {
    val seq = Seq()
    if(n==0){
      seq
    }
    else{
      for(i<-0 until n){
        seq :+ i
      }
      seq
    }

  }

  def funThree(n: Int): Int = {
    if(n==1){
      return n-1
    }
    if(n==10000){
      return n-3
    }
    if(n==10000){
      return n-1
    }
    n

  }

  def mapSum(n: Int, f: Int => Int): Int = {
 0
  }


}
