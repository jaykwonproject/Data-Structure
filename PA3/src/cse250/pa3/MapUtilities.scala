/**
 * cse250.pa3.MapUtilities.scala
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
 * citation: https://github.com/hartloff/CSE116-Examples/blob/master/src/week9/BFS.scala
 */
package cse250.pa3

import cse250.objects.{StreetGraph, TaxParcel}
import scala.collection.mutable
import scala.xml.XML

object MapUtilities {
  def loadIntersectionIDs(filename: String): mutable.Set[String] = {
    val osmFile = XML.loadFile(filename)
    val idSet = mutable.Set[String]()
    for(i<-osmFile.child){
      if(i.label=="node"){
        idSet += i.attributes("id").toString
      }
    }
    idSet
  }

  def loadMapInfo(filename: String): mutable.Map[String, mutable.Set[String]] = {
    val xmlFile = XML.loadFile(filename)
    val mapInfo = mutable.Map[String, mutable.Set[String]]()
    for(i<-xmlFile.child){ //under xml file
      if(i.label=="way"){
        for(j<-i.child){ //under way tag
          if(j.label=="nd"){
            val ids = j.attributes("ref").toString //ref ids
            for(x<-i.child){ //read again under way tag
              var streetSet = mutable.Set[String]()
              if(x.label=="tag"){
                if(x.attributes("k").toString == "tiger:name_base"){
                  var street = x.attributes("v").toString //street Name
                  if(!mapInfo.contains(ids)){ //if the map doesn't have the current intersectionId
                    streetSet += street
                    mapInfo.addOne(ids,streetSet)
                  }
                  else{ //if the map already has this intersection Id add the street to this id
                    for(y<-mapInfo){
                      if(y._1==ids){
                        y._2+=street
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    mapInfo
  }


  def buildIntersectionGraph(intersectionIDs: mutable.Set[String],
                             nodeToStreetMapping: mutable.Map[String, mutable.Set[String]]): StreetGraph = {

    val streetGraph = new StreetGraph
    for(i<-intersectionIDs){
      if(nodeToStreetMapping.contains(i)){
        if(nodeToStreetMapping(i).size>=2){
          for(j<-nodeToStreetMapping(i).toList){
            if(j!=""){
              streetGraph.insertVertex(j.toUpperCase)
            }
            for(x<-nodeToStreetMapping(i).toList.reverse){
              if(j!=x && j!="" && x!=""){
                streetGraph.insertEdge(j.toUpperCase,x.toUpperCase)
              }
            }
          }
        }
      }
    }
    streetGraph
  }
/*
  def computeFewestTurns(streetGraph: StreetGraph, start: TaxParcel, end: TaxParcel): Int = {
    val startSt = start.parcelInfo("STREET")
    val endSt = end.parcelInfo("STREET")
    var count = -1
    if(startSt==endSt){
      count = 0
    }
    else if(streetGraph.edges.contains(startSt,endSt)){
      count = 1
    }
    else{
      var newStart = ""
      var newEnd = ""
      for(i<-streetGraph.edges){
        if(i._1 == startSt){
          newStart = i._2
        }
        for(j<-streetGraph.edges){
          if(i!=j){
            if(j._2 == endSt){
              newEnd = j._1
              if(newStart==newEnd){
                count = 2
              }
              else if(newStart!=newEnd && streetGraph.edges.contains(newStart,newEnd)){
                count = 3
              }
            }
          }
        }
      }
    }
    count
  }
 */
  def computeFewestTurns(streetGraph: StreetGraph, start: TaxParcel, end: TaxParcel): Int = {
    val startSt = start.parcelInfo("STREET")
    val endSt = end.parcelInfo("STREET")
    var count = -1
    if(startSt==endSt){
      count = 0
    }
    else if(streetGraph.edges.contains(startSt,endSt)){
      count = 1
    }
    else{
      count = -1
    }
    count
  }

/*
  def computeFewestTurnsList(streetGraph: StreetGraph, start: TaxParcel, end: TaxParcel): Seq[String] = {
    var ls = mutable.ListBuffer[String]()
    val startSt = start.parcelInfo("STREET")
    val endSt = end.parcelInfo("STREET")

    if(startSt==endSt){
      ls+=(startSt)
    }
    else if(streetGraph.edges.contains(startSt,endSt)){
      ls+=(startSt)
      ls+=(endSt)
    }
    else{
      var newStart = ""
      var newEnd = ""
      for(i<-streetGraph.edges){
          if(i._1 == startSt){
            newStart = i._2
          }
          for(j<-streetGraph.edges){
            if(i!=j && ls.isEmpty){
              if(j._2 == endSt){
                newEnd = j._1
                if(newStart==newEnd){
                  ls+=(startSt)
                  ls+=(newStart)
                  ls+=(endSt)
                }
                else if(newStart!=newEnd && streetGraph.edges.contains(newStart,newEnd)){
                  ls+=(startSt)
                  ls+=(newStart)
                  ls+=(newEnd)
                  ls+=(endSt)
                }
              }
            }
          }
      }
    }
    ls.toSeq
  }
*/
  def computeFewestTurnsList(streetGraph: StreetGraph, start: TaxParcel, end: TaxParcel): Seq[String] = {
    List()
    val startSt = start.parcelInfo("STREET")
    val endSt = end.parcelInfo("STREET")

    if(startSt==endSt){
      List(startSt)
    }
    else if(streetGraph.edges.contains(startSt,endSt)){
      List(startSt,endSt)
    }
    else{
      List()
    }
  }
}
