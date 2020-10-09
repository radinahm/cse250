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
 * UBIT:radinahm
 * Person#:50273648
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
  // I implemented code from lecture of CSE 250 A section
  behavior of "GroupByStore.invariants 1(a)"
  it should "insert to head" in{
    val entries= AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME,AssessmentUtilities.DATA_ROWS)

    for(e<-entries){
      dataStore.insert(e)
      assert(dataStore._groupings.exists(p=>p._value.equals(e)))

    }
  }
  it should "insert to head of single list" in{
    val entries= AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME,AssessmentUtilities.DATA_ROWS)

    for(e<-entries){
      dataStore.insert(e)
      assert(dataStore._groupings.exists(p=>p._value.equals(e)))
      val first = dataStore._groupings.indexWhere(p=>p._value.equals(e))
      val last= dataStore._groupings.lastIndexWhere(p=>p._value.equals(e))
      assert(first==last)
    }
  }
  it should "insert into correct grouping" in{
    val entries= AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME,AssessmentUtilities.DATA_ROWS)

    for(e<-entries){
      dataStore.insert(e)
      assert(dataStore._groupings.exists(p=>p._value.parcelInfo(dataStore._groupingAttribute).equals(e.parcelInfo(dataStore._groupingAttribute))))
      val first = dataStore._groupings.indexWhere(p=>p._value.parcelInfo(dataStore._groupingAttribute).equals(e.parcelInfo(dataStore._groupingAttribute)))
      val last= dataStore._groupings.lastIndexWhere(p=>p._value.parcelInfo(dataStore._groupingAttribute).equals(e.parcelInfo(dataStore._groupingAttribute)))
      assert(first==last)
    }

  }
  
  behavior of "GroupByStore.invariants 1(b)"
  it should "sorted insert " in{
    val entries= AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME,AssessmentUtilities.DATA_ROWS)

    for(e<-entries) {
      dataStore.insert(e)
    }
      for (i<- 0 until dataStore._groupings.length-1){
        assert(dataStore._groupings(i)._value.parcelInfo(dataStore._groupingAttribute)<dataStore._groupings(i+1)._value.parcelInfo(dataStore._groupingAttribute))

      }
    }

  
  behavior of "GroupByStore.invariants 1(c)"
  it should "sort regroup" in{

    val entries= AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME,AssessmentUtilities.DATA_ROWS)
    for(e<-entries) {
      dataStore.insert(e)
    }
    dataStore.regroup("FRONT")
      for (i<- 0 until dataStore._groupings.length-1){
        assert(dataStore._groupings(i)._value.parcelInfo(dataStore._groupingAttribute)<dataStore._groupings(i+1)._value.parcelInfo(dataStore._groupingAttribute))
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

  }

  behavior of "GroupByStore.regroup"
  it should "regroup" in {
    val entries= AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME,AssessmentUtilities.DATA_ROWS)
    for(e<-entries){
      dataStore.insert(e)
    }
    val itt=dataStore.iterator
    dataStore.regroup("ZIP CODE (5-DIGIT)")


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
  it should "return false on empty" in{
    val itt = dataStore.iterator
    assert(itt.hasNext ==false)
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

    val datait = dataStore.iterator("AAAA")
    assert(!datait.hasNext)
  }
}
