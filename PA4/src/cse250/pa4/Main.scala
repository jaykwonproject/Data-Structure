package cse250.pa4

import scala.util.hashing.Hashing

object Main {
   def main(args: Array[String]): Unit = {
    val testSize = 10
    val inputKeys = Array.tabulate(testSize)(i => i + 1)
    val inputValues = Array.tabulate(testSize)(i => ('A' + i).toChar.toString)

   // val treeMap = new AVLTreeMap[Int, String]
    val hashMap = new HashTableMap[Int, String](0.5)(MyHasher)
    for (elem <- inputKeys.zip(inputValues)) {
      //treeMap.addOne(elem)
      hashMap.addOne(elem)
    }

    //println(treeMap.iterator.mkString("AVLTreeMap(", ",", ")"))
    println(hashMap.iterator.mkString("HashTableMap(", ",", ")"))
     for(i<-0 until 20){
       if(hashMap._bucketArray(i).isEmpty){
         println("empty")
       }
       else{
         println(hashMap._bucketArray(i).head)
       }
     }
  }
}

object MyHasher extends Hashing[Int] {
  override def hash(x: Int): Int = 3 * x * x + 2 * x + 5
}