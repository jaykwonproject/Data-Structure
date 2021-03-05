/**
 * cse250.pa2.SortedList.scala
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

import cse250.list.{ImmutableLinkedList,EmptyList,ListNode}
import cse250.adaptors.{LectureQueue,LectureStack}
import cse250.objects.TaxParcel


class SortedList[A] (implicit _comp: Ordering[A]) extends collection.Seq[A] {
  // Updates the toString to mention our class name instead of Seq.
  override protected[this] def className = "SortedList"
  var _numStored = 0
  // Use _storageList to maintain the sorted list.
  var _storageList: cse250.list.ImmutableLinkedList[A] = cse250.list.EmptyList
  // ---------- MAKE CHANGES BELOW ----------
  // You may add member variables as you wish.
  var _operationStack = new cse250.adaptors.LectureStack[ImmutableLinkedList[A]]
  var _checkBatch = new cse250.adaptors.LectureStack[Int]
  var _numstoredStack = new cse250.adaptors.LectureStack[Int]
  _operationStack.push(_storageList)
  _numstoredStack.push(_numStored)
  /** Gets element at position idx within the list. */
  override def apply(idx: Int): A = {
    if(_storageList.isEmpty){
      throw new IllegalArgumentException("the number must be non-negative.")
    }
    else{
      _storageList(idx)
    }
  }

  /** Gets the number of elements stored within the list. */
  override def length: Int = {
    _numStored
  }

  /** Returns an Iterator that can be used only once. */
  override def iterator: Iterator[A] = {
    _storageList.iterator
  }

  /**
   * Inserts one copy of elem into the list in non-decreasing order.
   * @param elem element to be inserted.
   */
  def insert(elem: A): Unit = {
    var ii = 0
    for(i<-0 to _storageList.length-1){
      if(_comp.compare(elem,_storageList(ii))>0){
        ii+=1
      }
    }

    _storageList = _storageList.inserted(ii,elem)
    _numStored +=1
    _operationStack.push(_storageList)
    _numstoredStack.push(_numStored)
    _checkBatch.push(0)
  }

  /**
   * Removes all copies of elem from the list.
   * @param elem element to be removed.
   * @return the number of copies removed.
   */
  def remove(elem: A): Int = {
    if(_storageList.isEmpty){
      0
    }
    else{
      var count= 0
      val listIterator = iterator
      var i = 0
      while(listIterator.hasNext){
        if(_comp.compare(elem,_storageList(i))==0){
          _storageList = _storageList.removed(i)
          count +=1
          listIterator.next()
        }
        else{
          listIterator.next()
          i+=1
        }
      }
      _numStored = _numStored-count
      if(count!=0){
        _operationStack.push(_storageList)
        _numstoredStack.push(_numStored)
      }
      _checkBatch.push(0)
      count
    }
  }

  /** Takes in a queue of valid operations to perform. Each pair has the form:
   *      (OP,elem)
   *  where:
   *      OP will be the string "insert" or "remove"
   *      elem will be a value of type A to use as the argument to OP. */
  def processBatch(operations: cse250.types.mutable.QueueADT[(String,A)]): Unit = {
    _checkBatch.push(1)
    while(operations.isEmpty==false){
      val instructions = operations.dequeue
      if(instructions._1=="insert"){
        insert(instructions._2)
      }
      else if(instructions._1=="remove"){
        remove(instructions._2)
      }
    }
    _checkBatch.push(1)
  }




  /** Undo the last modification, if any change has been made.
   *  If no change to undo exists, raise an IllegalArgumentException.
   */
  def undoLastModification(): Unit = {
    if(_operationStack.isEmpty==false){
      if(_checkBatch.pop == 1){
        while(_checkBatch.top==0){
          _checkBatch.pop
          _operationStack.pop
          _numstoredStack.pop
        }
        if(_numstoredStack.isEmpty==false && _operationStack.isEmpty==false){
          if(_storageList == _operationStack.top){
            _operationStack.pop
            _numstoredStack.pop
            _numStored = _numstoredStack.pop
            _storageList = _operationStack.pop
          }
          else{
            _numStored = _numstoredStack.pop
            _storageList = _operationStack.pop
          }
        }
        _checkBatch.pop
      }
      else{
        if(_numstoredStack.isEmpty==false && _operationStack.isEmpty==false) {
          _operationStack.pop
          _numstoredStack.pop
          _storageList = _operationStack.pop
          _numStored = _numstoredStack.pop
        }
      }

    }
    else{
      throw new IllegalArgumentException("can't undo empty list")
    }
  }

}

object NewIntOrdering extends Ordering[Int] {
  def compare(x: Int, y: Int): Int = {
    var retVal = 0
    if(x%2==0 && y%2==1){
      retVal = -1
    }
    else if(x%2==0 && y%2==0){
      if(x>y){
        retVal = -1
      }
      else if(x==y){
        retVal = 0
      }
      else{
        retVal = 1
      }
    }
    else if(x%2==1 && y%2==1){
      if(x>y){
        retVal = -1
      }
      else if(x==y){
        retVal = 0
      }
      else{
        retVal = 1
      }
    }
    else{
      retVal = 1
    }
    retVal
  }
}

object TaxParcelStreetGroupingOrdering extends Ordering[TaxParcel] {
  def compare(x: TaxParcel, y: TaxParcel): Int = {
    var retVal = 0
    if(x.parcelInfo("STREET")==y.parcelInfo("STREET")){
      retVal = 0
    }
    else if(x.parcelInfo("STREET")<y.parcelInfo("STREET")){
      retVal = -1
    }
    else{
      retVal = 1
    }
    retVal
  }
}
