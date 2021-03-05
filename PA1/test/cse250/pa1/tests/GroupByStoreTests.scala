/**
 * cse250.pa1.tests.GroupByStoreTests.scala
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
package cse250.pa1.tests

import cse250.objects.TaxParcel
import cse250.objects.AssessmentUtilities
import cse250.pa1.objects.GroupByStore
import org.scalatest.{BeforeAndAfter, FlatSpec}

import scala.collection.mutable

class GroupByStoreTests extends FlatSpec with BeforeAndAfter {
  var dataStore: GroupByStore = _

  // This code is run prior to every test.
  before {
    dataStore = new GroupByStore
  }

  // Your tests for problem 1 should be contained under this header.
  behavior of "GroupByStore.invariants 1(a)"
  it should "Insert TaxParcel at the head of respective grouping list" in {
    //insert parcels into dataStore
    val parcel = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, AssessmentUtilities.DATA_ROWS)
    for(i<-0 until parcel.length){
      dataStore.insert(parcel(i))
    }
    dataStore.regroup("SBL")
    for(i<-0 until parcel.length/2){
      dataStore.insert(parcel(i))
    }
    val name = "SBL"
    val lastParcelNum = parcel.length-1 //to identify the most recently added parcel
    val parcelName = parcel(lastParcelNum).parcelInfo(name) //name of the street of the most recently added parcel
    val grouping = dataStore._groupings

    for(i<-0 to grouping.length-1){
      val headName = grouping(i)._value.parcelInfo(name) //name of the street at each index
      if(parcelName == headName){  //if we're looking at the respective grouping list, meaning the index where the street name matches
        assert(grouping(i)._prev == null) //check if it's the head node
        assert(grouping(i)._value == parcel(lastParcelNum)) //if the head node's parcel is the newly added parcel

      }
    }

  }

  behavior of "GroupByStore.invariants 1(b)"
  it should "check if groupings are sorted after insert"in {
    val parcel = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, AssessmentUtilities.DATA_ROWS)
    for(i<-0 until parcel.length){
      dataStore.insert(parcel(i))
    }
    val grouping = dataStore._groupings
    val name = dataStore._groupingAttribute
    for(i<-0 to grouping.length-2){
      val current = grouping(i)._value.parcelInfo(name) //current header value String
      val next = grouping(i+1)._value.parcelInfo(name)  // next header value String
      assert(current<next) // organize alphabetically
    }
  }

  behavior of "GroupByStore.invariants 1(c)"
  it should "check _groupings are sorted after regroup"in {
    val parcel = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, AssessmentUtilities.DATA_ROWS)
    for(i<-0 until parcel.length){
      dataStore.insert(parcel(i))
    }

    val name = dataStore._groupingAttribute
    dataStore.regroup(name) //regroup by some attribute
    val grouping = dataStore._groupings
    for(i<-0 to grouping.length-2){ //same thing, check if it's ordered
      val current = grouping(i)._value.parcelInfo(name)
      val next = grouping(i+1)._value.parcelInfo(name)
      assert(current<next)
    }
  }

  // ^^^
  behavior of "GroupByStore.length"
  it should "be 0 when initialized" in {
    assert(dataStore.length == 0)
  }

  it should "be updated after each insertion" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, AssessmentUtilities.DATA_ROWS)
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
      assert(dataStore.length == i + 1)
    }
  }

  behavior of "GroupByStore.insert"
  it should "..." in {
    var a = 0
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, AssessmentUtilities.DATA_ROWS)
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
    }

      val headName = dataStore._groupings(2)._value.parcelInfo(dataStore._groupingAttribute)

      while(dataStore._groupings(2)._next!=null) {
        assert(headName==dataStore._groupings(2)._value.parcelInfo(dataStore._groupingAttribute))
        dataStore._groupings(2) = dataStore._groupings(2)._next
        a+=1
      }
    print(a)
  }

  behavior of "GroupByStore.regroup"
  it should "..." in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, AssessmentUtilities.DATA_ROWS)
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
    }
    dataStore.regroup("SBL")
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
    }
    dataStore.regroup("FRONT")
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
    }
    val name = "FRONT"
    val lastParcelNum = entries.length-1 //to identify the most recently added parcel
    val parcelName = entries(lastParcelNum).parcelInfo(name) //name of the street of the most recently added parcel
    val grouping = dataStore._groupings

    for(i<-0 to grouping.length-1){
      val headName = grouping(i)._value.parcelInfo(name) //name of the street at each index
      if(parcelName == headName){  //if we're looking at the respective grouping list, meaning the index where the street name matches
        assert(grouping(i)._prev == null) //check if it's the head node
        assert(grouping(i)._value == entries(lastParcelNum)) //if the head node's parcel is the newly added parcel

      }
    }

  }

  behavior of "GroupByStore.iterator"
  it should "retrieve all stored entries" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, AssessmentUtilities.DATA_ROWS)
    val testEntriesSet = new mutable.HashSet[TaxParcel]

    // Add all loaded values into your dataStore.
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
      testEntriesSet.add(entries(i))
    }

    // Check that all loaded values are iterated through in your dataStore.
    val dataIterator = dataStore.iterator
    val storedEntriesSet = new mutable.HashSet[TaxParcel]
    for (_ <- 0 until entries.length) {
      // dataIterator should still be valid.

      assert(dataIterator.hasNext)
      assert(dataIterator.hasNext)
      // Retrieve next element from sequence.
      val taxParcel = dataIterator.next
      // Check that entry was in the set of inserted entries.
      assert(testEntriesSet.contains(taxParcel))
      // Check that all entries are unique.
      assert(!storedEntriesSet.contains(taxParcel))
      storedEntriesSet.add(taxParcel)
    }
    assert(!dataIterator.hasNext)
  }

  it should "retrieve all stored entries after regrouping" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, AssessmentUtilities.DATA_ROWS)
    val testEntriesSet = new mutable.HashSet[TaxParcel]

    // Add all loaded values into your dataStore.
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
      testEntriesSet.add(entries(i))
    }

    // Check that all loaded values are iterated through in your dataStore.
    var dataIterator = dataStore.iterator
    var storedEntriesSet = new mutable.HashSet[TaxParcel]
    for (_ <- 0 until entries.length) {
      // dataIterator should still be valid.
      assert(dataIterator.hasNext)
      assert(dataIterator.hasNext)
      // Retrieve next element from sequence.
      val taxParcel = dataIterator.next
      // Check that entry was in the set of inserted entries.
      assert(testEntriesSet.contains(taxParcel))
      // Check that all entries are unique.
      assert(!storedEntriesSet.contains(taxParcel))
      storedEntriesSet.add(taxParcel)
    }
    assert(!dataIterator.hasNext)

    // Make a call to regroup.
    dataStore.regroup("SBL")

    // Check that all loaded values are iterated through in your dataStore.
    dataIterator = dataStore.iterator
    storedEntriesSet = new mutable.HashSet[TaxParcel]
    for (_ <- 0 until entries.length) {
      // dataIterator should still be valid.
      assert(dataIterator.hasNext)
      assert(dataIterator.hasNext)
      // Retrieve next element from sequence.
      val taxParcel = dataIterator.next
      // Check that entry was in the set of inserted entries.
      assert(testEntriesSet.contains(taxParcel))
      // Check that all entries are unique.
      assert(!storedEntriesSet.contains(taxParcel))
      storedEntriesSet.add(taxParcel)
    }
    assert(!dataIterator.hasNext)
  }

  behavior of "GroupByStore.iterator(String)"
  it should "..." in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, AssessmentUtilities.DATA_ROWS)
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
    }
    val dataIterator = dataStore.iterator("AAA")

    assert(dataIterator.hasNext)

  }
  behavior of "testing"
  it should "..." in {
    dataStore.insert(null)
    assertThrows[IndexOutOfBoundsException] { // Result type: Assertion
      dataStore._groupings.remove(0)
    }

  }



}
