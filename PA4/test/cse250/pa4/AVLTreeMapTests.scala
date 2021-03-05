/**
 * cse250.pa4.tests.AVLTreeMapTests.scala
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

package cse250.pa4.tests

import cse250.pa4.{AVLTree, AVLTreeMap}
import org.scalatest.FlatSpec


class AVLTreeMapTests extends FlatSpec {
  val testSize = 10
  val inputKeys = Array.tabulate(testSize)(i => i + 1)
  val inputValues = Array.tabulate(testSize)(i => ('A'+i).toChar.toString)
  behavior of "AVLTreeMap.insert"
  it should "add the (key,value) pairs" in {
    val treeMap = new AVLTreeMap[Int, String]
    val elements = inputKeys.zip(inputValues)
    for ((k, v) <- elements) {
      treeMap.addOne((k, v))
      assert(treeMap.contains(k))
    }
    val iterator = treeMap.iterator
    for (i <- elements.indices) {
      assert(iterator.hasNext)
      val elem = iterator.next
      assert(elem == elements(i))
    }
  }

  behavior of "Problem1.rotLeft."
  it should "rotateLeft with 3 nodes" in {
    val treeObj = new AVLTree[Int,String]
    val node = new treeObj.AVLNode[(Int,String)]((1,"A"),null, null, null, false, false)
    val node2 = new treeObj.AVLNode[(Int,String)]((2,"B"),null, null, null, false, false)
    val node3 = new treeObj.AVLNode[(Int,String)]((3,"C"),null, null, null, false, false)
    node3._parent = node2
    node2._right = node3
    node2._parent = node
    node._right = node2
    treeObj._avlRoot = node

    node._rightH = true
    node2._rightH = true


    val it = treeObj.iterator
    while(it.hasNext){
      print(it.next().toString()+" ")
    }
    treeObj.rotateLeft(node)

    println("")

    val it2 = treeObj.iterator
    while(it2.hasNext){
      print(it2.next().toString()+" ")
    }

/*
    assert(treeObj._avlRoot.eq(node2))
    assert(node2._left.eq(node))
    assert(node2._right.eq(node3))
    assert(node._parent.eq(node2))
    assert(node3._parent.eq(node2))
    assert(node._rightH == false)
    assert(node2._rightH == false)
*/

  }

  behavior of "Problem1.rotRight."
  it should "rotateRight with 3 nodes" in {
    val treeObj = new AVLTree[Int,String]
    val node = new treeObj.AVLNode[(Int,String)]((1,"A"),null, null, null, false, false)
    val node2 = new treeObj.AVLNode[(Int,String)]((2,"B"),null, null, null, false, false)
    val node3 = new treeObj.AVLNode[(Int,String)]((3,"C"),null, null, null, false, false)
    node._parent = node2
    node2._left = node
    node2._parent = node3
    node3._left = node2
    treeObj._avlRoot = node3

    node2._leftH = true
    node3._leftH = true

    treeObj.rotateRight(node3)

    assert(treeObj._avlRoot.eq(node2))
    assert(node2._left.eq(node))
    assert(node2._right.eq(node3))
    assert(node._parent.eq(node2))
    assert(node3._parent.eq(node2))
    assert(node2._leftH == false)
    assert(node3._leftH == false)
  }

  behavior of "Problem1.rotLeftRight."
  it should "rotateLeftRight with 3 nodes" in {
    val treeObj = new AVLTree[Int,String]
    val node = new treeObj.AVLNode[(Int,String)]((1,"A"),null, null, null, false, false)
    val node2 = new treeObj.AVLNode[(Int,String)]((2,"B"),null, null, null, false, false)
    val node3 = new treeObj.AVLNode[(Int,String)]((3,"C"),null, null, null, false, false)

    node2._parent = node
    node._right = node2
    node._parent = node3
    node3._left = node
    treeObj._avlRoot = node3

    node._rightH = true
    node3._leftH = true

    treeObj.rotateLeftRight(node3)

    assert(treeObj._avlRoot.eq(node2))
    assert(node2._left.eq(node))
    assert(node2._right.eq(node3))
    assert(node._parent.eq(node2))
    assert(node3._parent.eq(node2))
    assert(node._rightH == false)
    assert(node3._leftH  == false)
  }

  behavior of "Problem1.rotRightLeft."
  it should "rotateRightLeft with 3 nodes" in {
    val treeObj = new AVLTree[Int,String]
    val node = new treeObj.AVLNode[(Int,String)]((1,"A"),null, null, null, false, false)
    val node2 = new treeObj.AVLNode[(Int,String)]((2,"B"),null, null, null, false, false)
    val node3 = new treeObj.AVLNode[(Int,String)]((3,"C"),null, null, null, false, false)

    node2._parent = node3
    node3._left = node2
    node3._parent = node
    node._right = node3
    treeObj._avlRoot = node

    node._rightH = true
    node3._leftH = true

    treeObj.rotateRightLeft(node)

    assert(treeObj._avlRoot.eq(node2))
    assert(node2._left.eq(node))
    assert(node2._right.eq(node3))
    assert(node._parent.eq(node2))
    assert(node3._parent.eq(node2))
    assert(node._rightH == false)
    assert(node3._leftH  == false)
  }
}

