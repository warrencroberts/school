package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("lineOfSight should correctly handle an array of size 12") {
    val (yValues, tangents) = getTestData(12)
    val testOutput = new Array[Float](yValues.length)

    lineOfSight(yValues, testOutput)

    assert(testOutput.toList === List[Float](0.0f, 1.0f, 1.0f, 1.3333334f, 1.5f, 2.0f, 2.1666667f, 2.1666667f, 2.1666667f, 2.1666667f, 2.1666667f, 2.1666667f))
  }

  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }

  test("upsweepSequential should correctly handle the chunk larger array") {
    val (yValues, tangents)  = getTestData(12)
    val res = upsweepSequential(yValues, 0, 12)
    assert(res === 2.1666667f)
  }

  test("upsweepParallel should return same value as upsweekSequential") {
    val testResult =
      """Node(2.1666667)..l..Node(2.0)..l.....Leaf(0, 3, 1.0)..r.....Leaf(3, 6, 2.0)
        |..r..Node(2.1666667)..l.....Leaf(6, 9, 2.1666667)..r.....Leaf(9, 12, 1.6666666)
        |
        |""".stripMargin
    val (yValues, tangents) = getTestData(12)
    val upsweepTree = upsweep(yValues, 0, 12, 4)

//    println(s"upsweepTree =\n<<$upsweepTree>>")
//    println(s"testResult = \n<<$testResult>>")
    assert(s"<<$upsweepTree>>" === s"<<$testResult>>")
  }

  test("upsweepParallel should return same value as upsweepSequential for array 5 threshold 1") {
    val testResult =
      """Node(1.5)..l..Node(1.0)..l.....Leaf(1, 2, 1.0)..r.....Leaf(2, 3, 1.0)
        |..r..Node(1.5)..l.....Leaf(3, 4, 1.3333334)..r.....Leaf(4, 5, 1.5)
        |
        |""".stripMargin
    val (yValues, tangents) = getTestData(5)
    val upsweepTree = upsweep(yValues, 1, 5, 1)

//    println(s"upsweepTree =\n<<$upsweepTree>>")
//    println(s"testResult = \n<<$testResult>>")
    assert(s"<<$testResult>>" === s"<<$upsweepTree>>")
  }

  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("lineOfSight sequential vs parallel 5 element array threshold 1") {
    val (yValues, tangents) = getTestData(5)
    val testOutput = new Array[Float](yValues.length)

    lineOfSight(yValues, testOutput)

    val parOutput = new Array[Float](yValues.length)
    parLineOfSight(yValues, parOutput, 4)

    //    println(s"$testOutput")
    assert(testOutput === parOutput)
  }

  test("lineOfSight sequential vs parallel 12 element array threshold 4") {
    val (yValues, tangents) = getTestData(12)
    val testOutput = new Array[Float](yValues.length)

    lineOfSight(yValues, testOutput)

    val parOutput = new Array[Float](yValues.length)
    parLineOfSight(yValues, parOutput, 4)

    //    println(s"$testOutput")
    assert(testOutput === parOutput)
  }

  test("lineOfSight sequential vs parallel 12 element array threshold 1") {
    val (yValues, tangents) = getTestData(12)
    val testOutput = new Array[Float](yValues.length)

    lineOfSight(yValues, testOutput)

    val parOutput = new Array[Float](yValues.length)
    parLineOfSight(yValues, parOutput, 1)

    //    println(s"$testOutput")
    assert(testOutput === parOutput)
  }

  private def getTestData(arraySize: Int) = arraySize match {
    case 12 =>
      val yValues = Array[Float] (0f, 1f, 2f, 4f, 6f, 10f, 13f, 14f, 14f, 15f, 16f, 18f)
      val tangents = Array[Float] (1f / 1, 2f / 2, 4f / 3, 6f / 4, 10f / 5, 13f / 6, 14f / 7, 14f / 8, 15f / 9, 16f / 10, 18f / 11)
      (yValues, tangents)
    case 5 =>
      val yValues = Array[Float] (0f, 1f, 2f, 4f, 6f)
      val tangents = Array[Float] (1f / 1, 2f / 2, 4f / 3, 6f / 4).max
      (yValues, tangents)
  }
}

