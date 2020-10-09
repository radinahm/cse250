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
 * UBIT:radinahm
 * Person#:50273648
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa1.objects

import cse250.objects.{DNode, TaxParcel}

import scala.collection.mutable.ArrayBuffer
//I implemented code from the cse 250 repo for my hasnext and next also used code from lecture.
class GroupByStore extends Seq[TaxParcel] {
  // Member/instance variables are defined public for ease of access for testing.
  var _groupings: ArrayBuffer[DNode[TaxParcel]] = new ArrayBuffer[DNode[TaxParcel]]
  var _groupingAttribute: String = "STREET"
  var _numStored = 0

  def apply(i: Int): TaxParcel = {
    val iter = this.iterator
    for (_ <- 0 until i) iter.next()
    iter.next()
  }

  /** Inserts element to head of corresponding grouping list. */
  def insert(taxParcel: TaxParcel): Unit = {
    if(_numStored ==0){
      val _newnode= new DNode[TaxParcel](taxParcel,null,null)
      _groupings += _newnode
      _numStored +=1
    }
    else{
      if(_groupings.exists(p=>p._value.parcelInfo(_groupingAttribute).equals(taxParcel.parcelInfo(_groupingAttribute)))){
        val i = _groupings.indexWhere(p=>p._value.parcelInfo(_groupingAttribute).equals(taxParcel.parcelInfo(_groupingAttribute)))
        val newnode= new DNode[TaxParcel](taxParcel,null,_groupings(i))
        newnode._next._prev = newnode
        _groupings(i)= newnode
        _numStored+=1
      }
      else{
        val newnode= new DNode[TaxParcel](taxParcel,null,null)
        _groupings+= newnode
        _numStored +=1
      }


    }

    val sorted =_groupings.sortWith((a:DNode[TaxParcel],b:DNode[TaxParcel])=>a._value.parcelInfo(_groupingAttribute).toString<b._value.parcelInfo(_groupingAttribute).toString)
    _groupings = sorted

  }

  /** Regroup . */
  def regroup(attribute: String): Unit = {
    var _newheads: ArrayBuffer[DNode[TaxParcel]] = new ArrayBuffer[DNode[TaxParcel]]
    var numstored =0

    val itt = iterator

    if(attribute== _groupingAttribute){
      ;
    }
    else{

      while(itt.hasNext==true){
        if(numstored==0){
          val _newnode= new DNode[TaxParcel](itt.next(),null,null)
          _newheads+= _newnode
          numstored +=1
        }
        else{
          if(_newheads.exists(p=>p._value.parcelInfo(attribute).equals(itt.next().parcelInfo(attribute)))){
            val i = _newheads.indexWhere(p=>p._value.parcelInfo(attribute).equals(itt.next().parcelInfo(attribute)))
            println(i)
          }
        }
      }


    }
    _groupings = _newheads.sortWith((a:DNode[TaxParcel],b:DNode[TaxParcel])=>a._value.parcelInfo(attribute)<b._value.parcelInfo(attribute))

  }

  /** Returns an Iterator to all entries that can be used only once. */
  def iterator: Iterator[TaxParcel] = new Iterator[TaxParcel] {

    var index = 0
    var currentnode = new DNode[TaxParcel](null,null,null)
    if(_groupings.length!=0){
      currentnode= _groupings(index)
    }

    override def hasNext: Boolean = {
      if(_groupings.length==0){
        return  false

      }


      else{

        if(currentnode==null){
          if(index == _groupings.length-1){
            return false
          }
          else{
            index +=1
            currentnode = _groupings(index)
            return true

          }
        }
        else{

          true

        }

      }
    }

    override def next(): TaxParcel = {

      val retval = currentnode._value
      currentnode = currentnode._next
      retval
    }
  }

  /** Returns an Iterator to only the entries with matching values on the grouping attribute that can be used only once. */
  def iterator(value: String): Iterator[TaxParcel] = new Iterator[TaxParcel] {
    var index = 0
    var currentnode = new DNode[TaxParcel](null,null,null)
    if(_groupings.length!=0){
      currentnode= _groupings(index)
    }
    override def hasNext: Boolean ={
      if(_groupings.length==0){
        return  false

      }
      else{
        if(_groupings.exists(p=>p._value.parcelInfo(_groupingAttribute).equals(value))){
          index =_groupings.indexWhere(p=>p._value.parcelInfo(_groupingAttribute).equals(value))
          if(currentnode!=null){
            currentnode = _groupings(index)
            return true
          }
          else{
            false
          }

        }
        else {
          false
        }
      }
    }

    override def next(): TaxParcel = {
      val retval = currentnode._value
      currentnode = currentnode._next
      retval
    }
  }

  def length: Int = _numStored

  override def toString: String = this.iterator.mkString("GroupByStore(", "\n", ")")
}
