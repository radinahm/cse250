/**
 * cse250.pa1.objects.Main.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Modify at your leisure, but this will not be graded.
 */
package cse250.pa1.objects

import cse250.objects.{AssessmentUtilities, TaxParcel}

object Main {
  def main(args: Array[String]): Unit = {
    val taxParcelStore = new GroupByStore

    val numLines = 25 min AssessmentUtilities.DATA_ROWS
    for (entry <- AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, numLines)) {
      taxParcelStore.insert(entry)
    }
    println(s"Storage after $numLines additions:")
    println("-----")
    println(taxParcelStore)
    println("-----")

    taxParcelStore.regroup("NEIGHBORHOOD")

    println(s"Storage after regrouping by NEIGHBORHOOD:")
    println("-----")
    println(taxParcelStore)
    println("-----")

  }
}
