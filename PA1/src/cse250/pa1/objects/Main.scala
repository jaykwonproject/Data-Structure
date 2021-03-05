/**
 * cse250.pa1.objects.Main.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Modify at your leisure, but this will not be graded.
 */
package cse250.pa1.objects

import cse250.objects.{AssessmentUtilities, DNode, TaxParcel}

object Main {
  def main(args: Array[String]): Unit = {
    val taxParcelStore = new GroupByStore
    val it = taxParcelStore.iterator
    taxParcelStore._groupings(0) = new DNode[TaxParcel](null,null,null)

    /*
    val numLines = 25 min AssessmentUtilities.DATA_ROWS
    for (entry <- AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, numLines)) {
      taxParcelStore.insert(entry)
    }


    taxParcelStore.regroup("SBL")
    val parcel = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, AssessmentUtilities.DATA_ROWS)
    for(i<-0 until parcel.length){
    taxParcelStore.insert(parcel(i))
    }
    for(i<-0 to taxParcelStore._groupings.length-1){
      println(taxParcelStore._groupings(i)._value.parcelInfo("SBL"))
    }
 */


    /*
    taxParcelStore.regroup("NEIGHBORHOOD")
    println(s"Storage after some additions:") //$numLines
    println("-----")
    println(taxParcelStore._numStored)
    println(taxParcelStore._groupings.length)
    println("-----")


    for(i<-0 to taxParcelStore._groupings.length-1){
    println("Index:"+i+" = "+taxParcelStore._groupings(i)._value.parcelInfo("NEIGHBORHOOD"))
    }

    println("")
    println("-----")
    for(i<-0 to taxParcelStore._groupings.length-1){
    while(taxParcelStore._groupings(i)!=null) {
     print(taxParcelStore._groupings(i)._value.parcelInfo("NEIGHBORHOOD")+" -> ")
     taxParcelStore._groupings(i) = taxParcelStore._groupings(i)._next
    }
    println("")
    println("-----")
    }
    */


/*
    taxParcelStore.regroup("NEIGHBORHOOD")

    println(s"Storage after regrouping by NEIGHBORHOOD:")
    println("-----")
    println(taxParcelStore)
    println("-----")
*/
  }
}
