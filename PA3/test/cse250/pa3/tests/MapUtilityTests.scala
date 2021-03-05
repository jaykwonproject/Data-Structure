/**
 * cse250.pa3.tests.MapUtilityTests.scala
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
package cse250.pa3.tests

import org.scalatest.{BeforeAndAfter, FlatSpec}
import cse250.pa3.MapUtilities//added
import scala.collection.mutable//added
import cse250.objects.{StreetGraph, TaxParcel} //added
class MapUtilityTests extends FlatSpec with BeforeAndAfter {

  // Your tests for problem 1 should be contained under this header.
  behavior of "MapUtilityTests 1(a)"

  it should "1 matching intersection, 2 edges, 2 streets" in {
    val intersectionIDs = mutable.Set[String]("1", "2", "3")
    val mapInfo = mutable.Map[String, mutable.Set[String]]()
    mapInfo.addOne("1"-> mutable.Set("A","B"))
    val intersectionGraph = MapUtilities.buildIntersectionGraph(intersectionIDs,mapInfo)
    assert(intersectionGraph.edges.size==2)
    assert(intersectionGraph.vertices.size==2)
    assert(intersectionGraph.edges.contains("A","B"))
    assert(intersectionGraph.edges.contains("B","A"))
    assert(intersectionGraph.vertices.contains("A"))
    assert(intersectionGraph.vertices.contains("B"))
  }
  it should "2 matching intersections, 4 edges, 3 streets" in {
    val intersectionIDs = mutable.Set[String]("1", "2", "3")
    val mapInfo = mutable.Map[String, mutable.Set[String]]()
    mapInfo.addOne("1"-> mutable.Set("A","B"))
    mapInfo.addOne("2"-> mutable.Set("B","C"))
    val intersectionGraph = MapUtilities.buildIntersectionGraph(intersectionIDs,mapInfo)
    assert(intersectionGraph.edges.size==4)
    assert(intersectionGraph.vertices.size==3)
    assert(intersectionGraph.edges.contains("A","B"))
    assert(intersectionGraph.edges.contains("B","A"))
    assert(intersectionGraph.edges.contains("B","C"))
    assert(intersectionGraph.edges.contains("C","B"))
    assert(intersectionGraph.vertices.contains("A"))
    assert(intersectionGraph.vertices.contains("B"))
    assert(intersectionGraph.vertices.contains("C"))
  }
  it should "1 matching intersection, 2 edges, 3 streets" in {
    val intersectionIDs = mutable.Set[String]("1", "2", "3")
    val mapInfo = mutable.Map[String, mutable.Set[String]]()
    mapInfo.addOne("1"-> mutable.Set("A","B"))
    mapInfo.addOne("4"-> mutable.Set("B","C"))
    val intersectionGraph = MapUtilities.buildIntersectionGraph(intersectionIDs,mapInfo)
    assert(intersectionGraph.edges.size==2)
    assert(intersectionGraph.vertices.size==2)
    assert(intersectionGraph.edges.contains("A","B"))
    assert(intersectionGraph.edges.contains("B","A"))
    assert(intersectionGraph.vertices.contains("A"))
    assert(intersectionGraph.vertices.contains("B"))
  }
  it should "0 matching intersection, 0 edge, 2 streets" in {
    val intersectionIDs = mutable.Set[String]("1", "2", "3")
    val mapInfo = mutable.Map[String, mutable.Set[String]]()
    mapInfo.addOne("4" -> mutable.Set("A", "B"))
    val intersectionGraph = MapUtilities.buildIntersectionGraph(intersectionIDs, mapInfo)
    assert(intersectionGraph.edges.isEmpty)
    assert(intersectionGraph.vertices.isEmpty)
    assert(intersectionGraph.edges.size == 0)
    assert(intersectionGraph.vertices.size == 0)
  }

  it should "1 matching intersection, 0 edge, 0 streets" in {
    val intersectionIDs = mutable.Set[String]("1", "2", "3")
    val mapInfo = mutable.Map[String, mutable.Set[String]]()
    mapInfo.addOne("1" -> mutable.Set())
    val intersectionGraph = MapUtilities.buildIntersectionGraph(intersectionIDs, mapInfo)
    assert(intersectionGraph.edges.isEmpty)
    assert(intersectionGraph.vertices.isEmpty)
    assert(intersectionGraph.edges.size == 0)
    assert(intersectionGraph.vertices.size == 0)
  }

  it should "1 matching intersection, 6 edge, 3 streets" in {
    val intersectionIDs = mutable.Set[String]("1", "2", "3")
    val mapInfo = mutable.Map[String, mutable.Set[String]]()
    mapInfo.addOne("1"-> mutable.Set("A","B","C"))
    val intersectionGraph = MapUtilities.buildIntersectionGraph(intersectionIDs,mapInfo)
    assert(intersectionGraph.edges.size == 6)
    assert(intersectionGraph.vertices.size == 3)
  }

    
  behavior of "MapUtilityTests 1(b)"

  it should "return 0. Same Streets" in {
    val parcelStart = new TaxParcel
    val parcelFinish = new TaxParcel
    var startName = parcelStart.parcelInfo("STREET")
    var finishName = parcelFinish.parcelInfo("STREET")
    startName = "A"
    finishName = "A"
    val intersectionGraph = new StreetGraph
    intersectionGraph.insertVertex(startName)
    intersectionGraph.insertVertex(finishName)
    intersectionGraph.insertEdge(startName,finishName)
    intersectionGraph.insertEdge(finishName,startName)
    val turnNum = MapUtilities.computeFewestTurns(intersectionGraph,parcelStart,parcelFinish)
    if(startName.matches(finishName)){
      assert(turnNum == 0)
    }
}

  it should "return -1. Impossible to reach the other property" in {
    val parcelStart = new TaxParcel
    val parcelFinish = new TaxParcel
    var startName = parcelStart.parcelInfo("STREET")
    var finishName = parcelFinish.parcelInfo("STREET")
    startName = "A"
    finishName = "B"
    val intersectionGraph = new StreetGraph
    intersectionGraph.insertVertex(startName)
    intersectionGraph.insertEdge(startName,"nowhere")
    val turnNum = MapUtilities.computeFewestTurns(intersectionGraph,parcelStart,parcelFinish)
    assert(turnNum == -1)
  }

  it should "return 1. Two vertices and One Edge" in {
    val parcelStart = new TaxParcel
    val parcelFinish = new TaxParcel
    var startName = parcelStart.parcelInfo("STREET")
    var finishName = parcelFinish.parcelInfo("STREET")
    startName = "A"
    finishName = "B"
    val intersectionGraph = new StreetGraph
    intersectionGraph.insertVertex(startName)
    intersectionGraph.insertVertex(finishName)
    intersectionGraph.insertEdge(startName,finishName)
    val turnNum = MapUtilities.computeFewestTurns(intersectionGraph,parcelStart,parcelFinish)
    if(startName!=finishName){
      if(intersectionGraph.edges.contains((startName,finishName))){
        assert(turnNum == 1)
      }
    }
  }

  it should "return 3. 4 vertices and 3 Edge" in {
    val parcelStart = new TaxParcel
    val parcelFinish = new TaxParcel
    var startName = parcelStart.parcelInfo("STREET")
    var finishName = parcelFinish.parcelInfo("STREET")
    startName = "A"
    finishName = "B"
    val intersectionGraph = new StreetGraph
    intersectionGraph.insertVertex(startName)
    intersectionGraph.insertVertex("checkpoint1")
    intersectionGraph.insertVertex("checkpoint2")
    intersectionGraph.insertVertex(finishName)
    intersectionGraph.insertEdge(startName,"checkpoint1")
    intersectionGraph.insertEdge("checkpoint1","checkpoint2")
    intersectionGraph.insertEdge("checkpoint2","finishName")
    val turnNum = MapUtilities.computeFewestTurns(intersectionGraph,parcelStart,parcelFinish)
    if(startName!=finishName){
      if(!intersectionGraph.edges.contains((startName,finishName))){
        assert(turnNum == 3)
      }
    }
  }

// ^^^
behavior of "Other Functionality"
it should "..."
}

