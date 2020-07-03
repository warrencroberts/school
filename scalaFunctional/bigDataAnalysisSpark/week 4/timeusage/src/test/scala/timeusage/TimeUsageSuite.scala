package timeusage

import java.io.File

import org.apache.spark.sql.SparkSession
import org.apache.spark.sql._
import org.apache.spark.sql.functions.col
import org.apache.spark.sql.types._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}
import timeusage.TimeUsage.{spark, _}

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FlatSpec with Matchers with BeforeAndAfterAll {

  behavior of "TimeUsage.dfSchema"

  it must "Create struct type with first element string rest double" in {
    val columns = Source.fromFile("src/main/resources/timeusage/atussum.csv").getLines().next().split(",").toList

    val theSchema = TimeUsage.dfSchema(columns)
    theSchema.head.dataType shouldEqual StringType
    theSchema.tail.foreach(_.dataType shouldEqual DoubleType)
  }

  behavior of "TimeUsage.row"

  it must "Create row" in {
    val rowsLines = Source.fromFile("src/main/resources/timeusage/atussum.csv").getLines()

    val theSchema =TimeUsage.dfSchema(rowsLines.next.split(",").toList).toList

    val row = TimeUsage.row(rowsLines.next.split(",").toList).toSeq

    theSchema.zip(row).foreach { f =>
      f._2 match {
        case _: String => f._1.dataType shouldEqual StringType
        case _: Double => f._1.dataType shouldEqual DoubleType
        case x => assert(false, s"Unexpected type: $x")
      }
    }
  }

  behavior of "TimeUsage.classifiedColumns"

  def colTest(result: List[Column], arr: List[String]): Unit = {
    val testArray = arr.map(x => col(x))
    if(testArray.length < 3)
      result should contain allOf (testArray.head, testArray(1))
    else
      result should contain allOf (testArray.head, testArray(1), testArray.slice(2, testArray.length): _*)
  }

  it must "Classify Primary Needs" in {
    val columnNames = Source.fromFile("src/main/resources/timeusage/atussum.csv").getLines().next().split(",").toList

    @transient lazy val spark = SparkSession
      .builder()
      .config("spark.master", "local")
      .getOrCreate()

    val t01Cols = List("t010101","t010102","t010199","t010201","t010299","t010301","t010399","t010401","t010499","t010501","t010599","t019999")
    val t03Cols = List("t030101", "t030102", "t030103", "t030104", "t030105", "t030108", "t030109", "t030110", "t030111", "t030112", "t030186", "t030199", "t030201", "t030202", "t030203", "t030204", "t030299", "t030301", "t030302", "t030303", "t030399", "t030401", "t030402", "t030403", "t030404", "t030405", "t030499", "t030501", "t030502", "t030503", "t030504", "t030599", "t039999")
    val t11Cols = List("t110101", "t110199", "t110281", "t110289", "t119999")
    val t1801Cols = List("t180101", "t180199")
    val t1803Cols = List("t180381", "t180382", "t180399")

    val retColumns = TimeUsage.classifiedColumns(columnNames)._1

    colTest(retColumns, t01Cols)
    colTest(retColumns, t03Cols)
    colTest(retColumns, t11Cols)
    colTest(retColumns, t1801Cols)
    colTest(retColumns, t1803Cols)
  }

  it must "Classify Working Needs" in {
    val columnNames = Source.fromFile("src/main/resources/timeusage/atussum.csv").getLines().next().split(",").toList

    @transient lazy val spark = SparkSession
      .builder()
      .config("spark.master", "local")
      .getOrCreate()

    val t05Cols = List("t050101","t050102","t050103","t050189","t050201","t050202","t050203","t050204","t050289","t050301","t050302","t050303","t050304","t050389","t050403","t050404","t050405","t050481","t050499","t059999")
    val t1805Cols = List("t180501","t180502","t180589")

    val retColumns = TimeUsage.classifiedColumns(columnNames)._2

    colTest(retColumns, t05Cols)
    colTest(retColumns, t1805Cols)
  }

  behavior of "timeUsageSummary"

  it must "Compute timeUsageSummary properly" in {
    def round2dec(theVal: Double): Double = {
      math.round(theVal * 100) / 100.0
    }

    val (columns, initDf) = read("/timeusage/testData.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)

    val theData = summaryDf.select(col("working"), col("sex"), col("age"), col("primaryNeeds"), col("work"), col("other"))

    theData.show()
    val testWorking = List("working", "working", "working", "not working")
    val testSex = List("male", "female", "female", "female", "female")
    val testAge = List("elder", "active", "active", "active")
    val testPimaryNeeds = List(15.25, 13.83, 11.92, 13.08)
    val testWork = List(0,0,0,2)
    val testOther = List(8.75,10.17,12.08,8.92)
    theData.collect.zipWithIndex.foreach {x =>
      x._1.toSeq.head shouldEqual testWorking(x._2)
      x._1.toSeq(1) shouldEqual testSex(x._2)
      x._1.toSeq(2) shouldEqual testAge(x._2)
      round2dec(x._1.toSeq(3).asInstanceOf[Double]) shouldEqual testPimaryNeeds(x._2)
      round2dec(x._1.toSeq(4).asInstanceOf[Double]) shouldEqual testWork(x._2)
      round2dec(x._1.toSeq(5).asInstanceOf[Double]) shouldEqual testOther(x._2)
    }
  }

  behavior of "timeUsageGrouped"

  it must "Compute timeUsageGrouped properly" in {
    val (columns, initDf) = read("/timeusage/testData.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)
    val grouped = timeUsageGrouped(summaryDf)
    grouped.show()
  }

  behavior of "timeUsageGroupedSql"

  it must "Compute timeUsageGrouped properly" in {
    val (columns, initDf) = read("/timeusage/testData.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)
    val grouped = timeUsageGroupedSql(summaryDf)
    grouped.show()
  }

  behavior of "timeUsageSummaryTyped"

  it must "Compute timeUsageSummaryTyped properly" in {
    val (columns, initDf) = read("/timeusage/testData.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)
    val typed = timeUsageSummaryTyped(summaryDf)
    typed.show()
  }

  behavior of "timeUsageGroupedTyped"

  it must "Compute timeUsageSummaryTyped properly" in {
    val (columns, initDf) = read("/timeusage/testData.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)
    val typed = timeUsageSummaryTyped(summaryDf)
    val grouped = timeUsageGroupedTyped(typed)
    grouped.show()
  }

}
