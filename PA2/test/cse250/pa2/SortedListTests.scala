/**
 * cse250.pa2.SortedListTests.scala
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
package cse250.pa2

import cse250.adaptors.LectureQueue
import org.scalatest.{BeforeAndAfter, FlatSpec}

class SortedListTests extends FlatSpec with BeforeAndAfter {

  behavior of "iterator"

  it should "visit all elements in non-decreasing order" in {
    val myList = new SortedList[Int]
    val listIterator = myList.iterator
    assert(listIterator.hasNext == false) //check for empty/null
    myList.insert(5)
    myList.insert(1)
    myList.insert(2)
    myList.insert(1)
    myList.insert(5)

    val arrayLength = myList.length
    val tempArray = new Array[Int](arrayLength)
    for(i<-0 to myList.length-1){
      tempArray(i) = myList(i)
    }

    var i = 0
    //check the expected functionality of Iterator
    while(listIterator.hasNext){
      assert(listIterator.hasNext==true)
      val element = listIterator.next()
      tempArray(i) = element
      i+=1
    }
    //check the order
    for(x<-1 to arrayLength-1){
      val current = tempArray(x)
      val previous = tempArray(x-1)
      assert(current>=previous)
    }
  }


  behavior of "insert"
  it should "insert a solo element into list at index 0" in {
    val myList = new SortedList[Int]
    val valToInsert = 5
    myList.insert(valToInsert)
    assert(myList.length == 1)  //testing length method
    assert(myList(0) == valToInsert) //testing apply method and insert method
  }
  it should "insert multiple elements into list at index 0" in {
    val myList = new SortedList[Int]
    val valToInsert = 0
    myList.insert(1)
    myList.insert(valToInsert)
    assert(myList.length == 2)  //testing length method
    assert(myList(0) == valToInsert) //testing apply method and insert method

    for(i<- 1 to myList.length-1){
      val current = myList(i)
      val previous = myList(i-1)
      assert(current>=previous)
    }
  }
  it should "insert larger element last" in {
    val myList = new SortedList[Int]
    val valToInsert = 5
    myList.insert(1)
    myList.insert(2)
    myList.insert(valToInsert)
    assert(myList.length == 3)  //testing length method
    assert(myList(0) == 1) //testing apply method and insert method

    for(i<- 1 to myList.length-1){
      val current = myList(i)
      val previous = myList(i-1)
      assert(current>=previous)
    }
  }

  behavior of "remove"
  it should "check basic remove functionality" in {
    val myList = new SortedList[Int]
    myList.insert(1)
    myList.insert(2)
    myList.insert(2)
    myList.insert(3)
    val removeCount = myList.remove(2)
    val count = 2
    //also check how many copies are removed and remained.
    val listIterator = myList.iterator
    while(listIterator.hasNext){
      assert(listIterator.next()!=2)
      assert(removeCount == count)
    }
  }
  it should "removing non existing element" in {
    val myList = new SortedList[Int]
    myList.insert(1)
    myList.insert(2)
    myList.insert(3)
    val lengthBefore = myList.length
    val nonExistElement = myList.remove(4)
    val count = 0
    assert(nonExistElement == count)
    assert(myList.length == lengthBefore)
  }

  it should "removing an empty list" in {
    val myList = new SortedList[Int]
    assert(myList.length==0)
    assert(myList.remove(0) == 0)
  }





  behavior of "processBatch"
  it should "process two insertions" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",0)
    jobQueue.enqueue("insert",0)
    myList.processBatch(jobQueue)
    // Should have inserted the values: 0,0.
    assert(myList.length == 2)
    assert(myList(0) == 0)
    assert(myList(1) == 0)
    // Should have removed both copies of 0.
    jobQueue.enqueue("remove",0)
    myList.processBatch(jobQueue)
    assert(myList.length == 0)
  }

  it should "process 3 insertions and 2 removals" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",0)
    jobQueue.enqueue("insert",1)
    jobQueue.enqueue("insert",2)
    jobQueue.enqueue("remove",1)
    jobQueue.enqueue("remove",2)
    myList.processBatch(jobQueue)
    assert(myList.length == 1)
    assert(myList(0) == 0)
  }

  it should "not result in a change to undo if no modification is made" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",1)
    jobQueue.enqueue("remove",1)
    myList.processBatch(jobQueue)
    assert(myList.length == 1)
    assert(myList(0) == 0)
  }

  it should "process empty queue into non-empty list" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    myList.insert(1)
    val jobQueue = new LectureQueue[(String,Int)]
    myList.processBatch(jobQueue)
    assert(myList.length == 2)
    assert(myList(0) == 0)
    assert(myList(1) == 1)
  }

  it should "process empty queue into empty list" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    myList.processBatch(jobQueue)
    assert(myList.length == 0)
    assertThrows[IllegalArgumentException] {
      myList(0)
    }
  }



  behavior of "undoLastModification"

  it should "undo after single insertion" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    myList.undoLastModification()
    assert(myList.length == 0)
  }
  it should "after removing nothing" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    myList.remove(1) //non-existent
    myList.undoLastModification()
    assert(myList.length == 0)
  }
  it should "undo multiple insertions" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    myList.insert(1)
    myList.insert(2)
    myList.insert(3)
    myList.insert(4)
    myList.undoLastModification()
    assert(myList.length == 4)
  }
  it should "undo after removing unique value" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    myList.insert(1)
    myList.insert(2)
    myList.insert(3)
    myList.insert(4)
    myList.remove(2)
    assert(myList.length == 4)
    assert(myList.contains(2)==false)
    myList.undoLastModification()
    assert(myList.length == 5)
    assert(myList.contains(2))
  }
  it should "undo after multiple insertions/removals with duplicates" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    myList.insert(0)
    myList.insert(1)
    myList.insert(1)
    myList.insert(2)
    myList.insert(2)
    myList.remove(1)
    assert(myList.length == 4)
    assert(myList.contains(1)==false)
    myList.undoLastModification()
    assert(myList.length == 6)
    assert(myList.contains(1))
  }
  it should "process batch, undo and batch again" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",0)
    myList.processBatch(jobQueue) //0
    assert(myList.length == 1)
    myList.undoLastModification() //nothing
    assert(myList.length == 0)
  }

  it should "undo when empty/no modifiction to undo" in {
    val myList = new SortedList[Int]
    assertThrows[IllegalArgumentException] {
      myList.undoLastModification()
    }
  }
  it should "undoing non-empty list after removing nonexsiting element" in {
    val myList = new SortedList[Int]
    myList.remove(5)
    assertThrows[IllegalArgumentException] {
      myList.undoLastModification()
    }

  }
  it should "batch first with removing nonexisting element and undoing" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",0)
    jobQueue.enqueue("remove",1) //would skip
    myList.processBatch(jobQueue)
    assert(myList.length == 1) //0
    assert(myList.contains(0))
    myList.undoLastModification()
    assert(myList.length ==0)
  }



}

