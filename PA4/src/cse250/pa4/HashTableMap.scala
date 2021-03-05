/**
 * cse250.pa4.HashTableMap.scala
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
package cse250.pa4

import cse250.examples.types.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.util.hashing.Hashing

class HashTableMap[K, V](val alphaMax: Double = 0.6)(implicit hash: Hashing[K]) extends Map[K, V] {
  var _n = 0 //number of elements
  var _N = 10 //size of the array
  var _alpha: Double = 0.0 // (n/M)
  var _bucketArray = Array.fill[ListBuffer[(K, V)]](_N)(ListBuffer[(K, V)]()) //hashtable array
  def rehash(newSize: Int): Unit = {
    if (newSize > _N) {
      val oldBucketArray = _bucketArray
      _n = 0
      _N = newSize
      _alpha = 0.0
      _bucketArray = Array.fill(_N)(ListBuffer[(K, V)]())
      for (bucket <- oldBucketArray; elem <- bucket) this.addOne(elem)
    }
  }

  override def get(key: K): Option[V] = {
    val lookupIndex = hash.hash(key) % _N
    _bucketArray(lookupIndex).find(elem => elem._1 == key) match {
      case Some(elem) => Some(elem._2)
      case None       => None
    }
  }

  override def addOne(elem: (K, V)): Unit = {
    val index = hash.hash(elem._1) % _N
    _n+=1
    _alpha = _n.toFloat/_N
    if(_alpha>=alphaMax){
      rehash(_N*2)
    }
    _bucketArray(index).prepend(elem)
  }

  override def removeOne(key: K): Boolean = {
    val index = hash.hash(key) % _N
    if (_bucketArray(index).nonEmpty) {
      if (_bucketArray(index).head._1 == key) {
        _bucketArray(index).clear()
        _n -=1
        _alpha = _n.toFloat/_N
        true
      }
      else{
        false
      }
    }
      else {
        false
      }
  }

  override def iterator: Iterator[(K, V)] = {
    var check = 0
    val myListBuffer : ListBuffer[(K,V)] = ListBuffer[(K, V)]()
    for(x <- _bucketArray.indices) {
      var i = 0
      var bool = true
      while(i < _bucketArray(x).length && !_bucketArray.isEmpty) {
        if(_bucketArray(x).isEmpty) {
          bool = false
        }
        else {
          myListBuffer.insert(check,_bucketArray(x)(i))
          check += 1
        }
        i += 1
      }
    }
    myListBuffer.iterator
  }
}
