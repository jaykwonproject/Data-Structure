/**
 * AssessmentDataProcessorTests.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT:
 * Person#:
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa0.tests

import cse250.pa0.objects.AssessmentDataProcessor
import org.scalatest.FlatSpec

class AssessmentDataProcessorTests extends FlatSpec {
  behavior of "AssessmentDataProcessor.sanitizeData"

  it should "..." in {
    AssessmentDataProcessor.sanitizeData("data/2019-2020_Assessment_Roll.csv")
  }
}
