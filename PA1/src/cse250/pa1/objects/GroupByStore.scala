/**
 * cse250.pa1.GroupByStore.scala
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
package cse250.pa1.objects

import cse250.objects.{DNode, TaxParcel}

import scala.collection.mutable.ArrayBuffer

class GroupByStore extends Seq[TaxParcel] {
  // Member/instance variables are defined public for ease of access for testing.
  var _groupings: ArrayBuffer[DNode[TaxParcel]] = new ArrayBuffer[DNode[TaxParcel]]
  var _groupingAttribute: String = "STREET"
  var _numStored = 0
  private var _headNode: DNode[TaxParcel] = null
  private var _tailNode: DNode[TaxParcel] = null

  def apply(i: Int): TaxParcel = {
    val iter = this.iterator
    for (_ <- 0 until i) iter.next()
    iter.next()
  }

  /** Inserts element to head of corresponding grouping list. */
  def insert(taxParcel: TaxParcel): Unit = {
    var count = 0
    if(_numStored == 0){
      _headNode = new DNode (taxParcel,null,null)
      _groupings += _headNode
      _tailNode = _headNode
      _numStored+=1
    }
    else{
      for(i<-0 to _groupings.length-1){
        if(_groupings(i)._value.parcelInfo(_groupingAttribute) == taxParcel.parcelInfo(_groupingAttribute)){
          _headNode = new DNode(taxParcel, null, _groupings(i))
          _groupings(i) = _headNode
          _headNode._next._prev = _headNode
          count = 1
        }
      }
      if(count == 0){
        _headNode = new DNode (taxParcel,null,null)
        _groupings += _headNode
        _tailNode = _headNode
        var i = 0
        while(i!=_groupings.length-1){
          if(_groupings(i)._value.parcelInfo(_groupingAttribute)>_groupings(i+1)._value.parcelInfo(_groupingAttribute)){
            val temp = _groupings(i)
            _groupings(i) = _groupings(i+1)
            _groupings(i+1) = temp
            i = 0
          }
          else{
            i+=1
          }
        }
      }
      _numStored+=1
    }
    //maintaining the ordering as parcels are inserted
  }

  /** Regroup . */
  def regroup(attribute: String): Unit = {
    if(attribute!=_groupingAttribute && _numStored!=0){
      var _temp: ArrayBuffer[TaxParcel] = new ArrayBuffer[TaxParcel]
      val it = iterator
      while(it.hasNext){
        _temp+=it.next()
      }
      _groupings.clearAndShrink(0)
      _numStored=0
      _groupingAttribute = attribute
      for(i<-0 to _temp.length-1){
        insert(_temp(i))
      }
    }
    else if(_numStored==0){
      _groupingAttribute = attribute
    }
  }

  /** Returns an Iterator to all entries that can be used only once. */
  def iterator: Iterator[TaxParcel] = new Iterator[TaxParcel] {
    var arrayNum = 0
    var parcel : DNode[TaxParcel] = null
    if(_numStored!=0){
      parcel = _groupings(arrayNum)
    }
    override def hasNext: Boolean = {
     if(_numStored!=0) {
       if(parcel == null && arrayNum+1!=_groupings.length){
         arrayNum+=1
         parcel = _groupings(arrayNum)
       }
      parcel!=null && _groupings.length!=0
     }
      else{
       false
     }
    }
    override def next(): TaxParcel = {
      val retVal = parcel._value
      parcel = parcel._next
      retVal
    }
  }

  /** Returns an Iterator to only the entries with matching values on the grouping attribute that can be used only once. */
  def iterator(value: String): Iterator[TaxParcel] = new Iterator[TaxParcel] {
    var idxNum = 0
    var count = 0
    for(i<-0 to _groupings.length-1){
      if(value == _groupings(i)._value.parcelInfo(_groupingAttribute)){
        idxNum = i
        count +=1
      }
    }
    var parcel : DNode[TaxParcel] = null
    if(_numStored!=0){
      parcel = _groupings(idxNum)
    }
    override def hasNext: Boolean = {
      if(_numStored!=0 && count!=0) {
        idxNum >= 0 && parcel != null
      }
      else{
        false
      }
    }
    override def next(): TaxParcel = {
      val retVal = parcel._value
      parcel = parcel._next
      retVal
    }
  }



  def length: Int = _numStored

  override def toString: String = this.iterator.mkString("GroupByStore(", "\n", ")")
}
