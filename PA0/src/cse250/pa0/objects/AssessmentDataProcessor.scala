/**
 * cse250.pa0.objects.AssessmentDataProcessor.scala
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
 * UBIT:changhos
 */
package cse250.pa0.objects

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

import cse250.objects.TaxParcel

import scala.io.Source

object AssessmentDataProcessor {
  def sanitizeData(filename: String): Unit = {
    val inputFile = scala.io.Source.fromFile(filename) //open the file
    val outputFile = new BufferedWriter(new FileWriter( new File(filename + "-updated"))) //make sure to use only -updated when submitting!
    val lines = inputFile.getLines() //read the header row!
    val cols = List(10,11,12,13,14,17,21,22,23,24,25,30,31,32,33,34,35,36,37,38,41,44,45,46) //columns that need to be removed from each row.

    //header section
    val header = lines.next.split(',')  //split the cells by comma
    for(i<-0 to header.length-1){
      if(cols.contains(i)==false){ //if the header isn't one of the column numbers given
        outputFile.write(header(i)) //then write it in the new file
        if(i!=header.length-1){ //and if it's not the last header
          outputFile.write(',') //continue writing comma afterwards to separate
        }
      }
    }
    outputFile.write('\n')

    //value section
    while(lines.hasNext){ //now gotta read the whole values until the end
      val values = lines.next.split('\n') //each value row since every string ends with a new line
      val words = new StringBuilder(values.mkString) //make that row into one long string value
      var i = 1 //index for reading each char
      var count = 0 //count value for quotations

      while(i<words.length()-1){
        if(words(i)=='\"'){ //if the quotation mark appears
          count += 1 //count for quotation increments
          i+=1 //do nothing, and move on to the next char
        }
        else if(words(i-1)=='\"'&& words(i)==',' && count %2==0){ //check if it's the end of the quotation
          words(i)= '!' //replace that comma with ! to distinguish the commas within the quotation
          i+=1 //move on to the next char
          count = 0 //reset the count
        }
        else if(words(i)==',' && count==0){ //else if char equals comma AND it's not within the quotation
          words(i)= '!' //replace that comma with ! to distinguish the commas within the quotation
          i+=1 //move on to the next char
        }
        else{ //otherwise just move on to the next char
          i+=1
        }
      }

      val a = words.mkString.split('!') //now that each cell is divided by ! instead of comma, each cells are distinguishable from one another
      for(i<-0 to a.length-1){
        if(cols.contains(i)==false){
          outputFile.write(a(i))
          if(i!=a.length-1){
            outputFile.write(',')
          }
        }
      }
      outputFile.write("\n")
    }

    inputFile.close()
    outputFile.close()
  }

  def computeOldestEntry(filename: String): TaxParcel = {
    val inputFile = scala.io.Source.fromFile(filename)
    val lines = inputFile.getLines()
    val tpacel = new TaxParcel //instantiate TaxParcel Class to create an object
    val pInfo = tpacel.parcelInfo //call parcelInfo method to use the map function
    val taxHeaders = TaxParcel.HEADERS //calling the object, not the class!
    var yearIndex = 0 //index for the column of index where 'year built' is located

    //header section
    val header = lines.next.split(',')
    for(i<-0 to header.length-1){
      if(header(i)=="YEAR BUILT"){
        yearIndex = i //found the index!
      }
    }

    //value section
    var oldest = 2020 //assuming that the most recent building is built in this year 2020
    while(lines.hasNext){ //same algorithm to distinguish each cell
      val values = lines.next.split('\n')
      val words = new StringBuilder(values.mkString)
      var i = 1
      var count = 0
      while(i<words.length()-1){
        if(words(i)=='\"'){
          count += 1
          i+=1
        }
        else if(words(i-1)=='\"'&& words(i)==',' && count %2==0){ //check if it's the end of the quotation
          words(i)= '!' //replace that comma with ! to distinguish the commas within the quotation
          i+=1 //move on to the next char
          count = 0 //reset the count
        }
        else if(words(i)==',' && count==0){ //else if char equals comma AND it's not within the quotation
          words(i)= '!' //replace that comma with ! to distinguish the commas within the quotation
          i+=1 //move on to the next char
        }
        else{
          i+=1
        }
      }

      val a = words.mkString.split('!')
      for(j<-0 to a.length-1){
        if(a(yearIndex)!="NA"&& a(yearIndex)!="" && a(yearIndex).toInt > 999 && a(yearIndex).toInt < 2021){ //if the cell actually contains a 4-digit number
          if(a(yearIndex).toInt<oldest){ //then lower the number is, the older it is
            oldest = a(yearIndex).toInt //save this year to comapre with other years in other rows
            for(k<-0 to a.length-1){
              pInfo+=(taxHeaders(k)->a(k)) //Hashmap("Year Built" -> 1780, ... and so on)
            }
          }
        }
      }
    }
    println(pInfo("STREET"))
    inputFile.close()
    tpacel //returning the TaxParcel
  }

  def countPriceRange(filename: String, lower: Int, upper: Int): Int = {
    val inputFile = scala.io.Source.fromFile(filename)
    val lines = inputFile.getLines()
    var totalv = 0 //index where total value is located in the headers
    var num = 0

    //header section
    val header = lines.next.split(',')
    for(i<-0 to header.length-1){
      if(header(i)=="TOTAL VALUE"){
        totalv = i
      }
    }

    //value section
    while(lines.hasNext){
      val values = lines.next.split('\n') //each value row
      val words = new StringBuilder(values.mkString)
      var i = 1
      var count = 0
      while(i<words.length()-1){
        if(words(i)=='\"'){
          count += 1
          i+=1
        }
        else if(words(i-1)=='\"'&& words(i)==',' && count %2==0){ //check if it's the end of the quotation
          words(i)= '!' //replace that comma with ! to distinguish the commas within the quotation
          i+=1 //move on to the next char
          count = 0 //reset the count
        }
        else if(words(i)==',' && count==0){ //else if char equals comma AND it's not within the quotation
          words(i)= '!' //replace that comma with ! to distinguish the commas within the quotation
          i+=1 //move on to the next char
        }
        else{
          i+=1
        }
      }

      val a = words.mkString.split('!')
      if(a(totalv)!="NA"&& a(totalv)!="" &&a(totalv).toInt>=lower && a(totalv).toInt<upper){ //if it's in the range
          num+=1 //then count the parcel!
      }
    }
    inputFile.close()

    num //returning the number of parcel
  }
}
