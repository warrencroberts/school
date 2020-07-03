package kmeans

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import scala.collection.{GenMap, GenSeq}
import DoubleUtils._

object KM extends KMeans
import kmeans.KM._

@RunWith(classOf[JUnitRunner])
class KMeansSuite extends FunSuite {

  def checkClassify(points: GenSeq[Point],
                    means: GenSeq[Point],
                    expected: GenMap[Point, GenSeq[Point]]) {
    assert(classify(points, means) == expected,
           s"classify($points, $means) should equal to $expected")
  }

  def checkUpdate(classified: GenMap[Point, GenSeq[Point]],
                  oldMeans: GenSeq[Point],
                  expectedMeans: GenSeq[Point]) {
    def compareLists(newPoints: List[Point],
                     expectedPoints: List[Point]): Boolean = {
      newPoints
        .zip(expectedPoints)
        .forall((pt) =>
          (pt._1.x ~= pt._2.x) && (pt._1.y ~= pt._2.y) && (pt._1.z ~= pt._2.z))
    }

    val updatedMeansList = update(classified, oldMeans).toList
    val expectedMeansList = expectedMeans.toList

    assert(compareLists(updatedMeansList, expectedMeansList),
           s"\n$updatedMeansList should equal to $expectedMeansList")
  }

  def checkParClassify(points: GenSeq[Point],
                       means: GenSeq[Point],
                       expected: GenMap[Point, GenSeq[Point]]) {
    assert(classify(points.par, means.par) == expected,
           s"\nclassify($points par, $means par) should equal to $expected")
  }

  def genPoints(points: List[(Double, Double, Double)]): GenSeq[Point] = {
    points.foldLeft(IndexedSeq[Point]()) { (x, p) =>
      x :+ new Point(p._1, p._2, p._3)
    }
  }

  test("'classify should work for empty 'points' and empty 'means'") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq()
    val expected = GenMap[Point, GenSeq[Point]]()
    checkClassify(points, means, expected)
  }

  test(
    "'classify should work for  'points' == GenSeq(Point(1,1,1)) and empty 'means'") {
    val points: GenSeq[Point] = genPoints(List((1, 1, 1)))
    val means: GenSeq[Point] = genPoints(List())
    val expected = GenMap[Point, GenSeq[Point]]()
    checkClassify(points, means, expected)
  }

  test(
    "'classify' should work for empty 'points' and 'means' == GenSeq(Point(1,1,1))") {
    val points: GenSeq[Point] = IndexedSeq()
    val means = genPoints(List((1, 1, 1)))
    val expected = GenMap[Point, GenSeq[Point]]((means.head, GenSeq()))
    checkClassify(points, means, expected)
  }

  test(
    "'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((0, 0, 0))") {
    val points: GenSeq[Point] =
      genPoints(List((1,1,0), (1,-1,0), (-1,1,0), (-1,-1,0)))

    val means = genPoints(List((0,0,0)))

    val expected = GenMap(
      (means.head, GenSeq(points.head, points(1), points(2), points(3))))
    checkClassify(points, means, expected)
  }

  test(
    "'classify' should work for 'points' == GenSeq((1,1,0), (1,-1,0), (-1,1,0), (-1,-1,0)) and 'means' == GenSeq((1,0,0), (-1,0,0))") {
    val points =
      genPoints(List((1,1,0), (1,-1,0), (-1,1,0), (-1,-1,0)))

    val means = genPoints(List((1,0,0), (-1,0,0)))
    val expected = GenMap((means.head, GenSeq(points.head, points(1))),
                          (means(1), GenSeq(points(2), points(3))))
    checkClassify(points, means, expected)
  }

  test(
    "'classify with data parallelism should work for empty 'points' and empty 'means'") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq()
    val expected = GenMap[Point, GenSeq[Point]]()
    checkParClassify(points, means, expected)
  }

  test(
    "classify with points (1,1,0), (2,1,0), (1,2,0), (5,3,0), (6,5,0), (6,4,0) means (2,3,0), (4,4,0) results in ((2,3,0) -> ((1,1,0), (2,1,0), (1,2,0)), (4,4,0) -> ((5,3,0), (6,5,0), (6,4,0))") {
    val points = genPoints(
      List((1,1,0), (2,1,0), (1,2,0), (5,3,0), (6,5,0), (6,4,0)))
    val means = genPoints(List((2,3,0), (4,4,0)))

    val expected =
      GenMap(means.head -> GenSeq(points.head, points(1), points(2)),
             means(1) -> GenSeq(points(3), points(4), points(5)))
    checkClassify(points, means, expected)

    val expectedMeans =
      genPoints(List((4.0 / 3, 4.0 / 3, 0), (17.0 / 3, 12.0 / 3, 0)))
    checkUpdate(expected, means, expectedMeans)
  }

  test("converged with means old (1,1,0) new  (1,2,0) distance = 1") {
    val oldMeans = genPoints(List((1,1,0)))
    val newMeans = genPoints(List((1,2,0)))

    //    val distance = oldMeans.head.squareDistance(newMeans.head)
    //
    assert(converged(1.0)(oldMeans, newMeans))
  }

  test("not converged with means old (1,1,0) new  (1,3,0) distance = 2") {
    val oldMeans = genPoints(List((1,1,0)))
    val newMeans = genPoints(List((1,3,0)))

    val distance = oldMeans.head.squareDistance(newMeans.head)

    assert(!converged(1.0)(oldMeans, newMeans))
  }

  test("converged with means oldMeans ((1,1,0), (2,2,0)) newMeans ((1,1,0), (2,3,0)) distance = 1") {
    val oldMeans = genPoints(List((1,1,0), (2,2,0)))
    val newMeans = genPoints(List((1,2,0), (2,3,0)))

    val distance = oldMeans(1).squareDistance(newMeans(1))

    assert(converged(1.0)(oldMeans, newMeans))
  }

  test("not converged with means oldMeans ((1,1,0), (2,2,0)) newMeans ((1,1,0), (2,4,0)) distance = 2") {
    val oldMeans = genPoints(List((1,1,0), (2,2,0)))
    val newMeans = genPoints(List((1,2,0), (2,4,0)))

    assert(!converged(1.0)(oldMeans, newMeans))
  }

  test("kmeans converged with points ((1,1,0), (2,2,0)) means ((10,20,0))") {
    val points = genPoints(List((1,1,0), (2,2,0)))
    val means = genPoints(List((10,20,0)))

    val resultMeans = kMeans(points, means, 1.0)
    val testMeans = List(new Point(1.5,1.5,0.0))
    assert((resultMeans zip testMeans).forall(x => x._1 == x._2))
  }

}
