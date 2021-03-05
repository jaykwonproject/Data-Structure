/**
 * cse250.pa0.objects.Main.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 *
 */
package cse250.pa0.objects

object Main {
  def main(args: Array[String]): Unit = {
    val filename = "data/2019-2020_Assessment_Roll.csv"
    AssessmentDataProcessor.sanitizeData(filename)
    println(AssessmentDataProcessor.computeOldestEntry(filename + "-updated").toString)
    println(AssessmentDataProcessor.countPriceRange(filename + "-updated",0, 1000000))
  }
}
