package barneshut

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import scala.math._
import scala.collection.parallel._
import barneshut.conctrees.ConcBuffer

@RunWith(classOf[JUnitRunner])
class BarnesHutSuite extends FunSuite {

  def genSectorMatrix(xMin: Float, yMin: Float, xMax: Float, yMax: Float, precision: Int): SectorMatrix = {
    val boundaries = new Boundaries()
    boundaries.minX = xMin
    boundaries.maxX = xMax
    boundaries.minY = yMin
    boundaries.maxY = yMax
    new SectorMatrix(boundaries, precision)
  }

  // test cases for quad tree

  import FloatOps._

  test("Empty: center of mass should be the center of the cell") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  test("Empty: mass should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  test("Fork with 3 empty quadrants and 1 leaf (nw)") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.size.toFloat == 10f, s"${quad.size} should be 10f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Fork with 3 empty quadrants and 1 leaf (ne)") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Leaf(22.5f, 27.5f, 5f, Seq(b))
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.size.toFloat == 10f, s"${quad.size} should be 10f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Fork with 3 empty quadrants and 1 leaf (sw)") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Leaf(17.5f, 32.5f, 5f, Seq(b))
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.size.toFloat == 10f, s"${quad.size} should be 10f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Fork with 3 empty quadrants and 1 leaf (se)") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Leaf(22.5f, 32.5f, 5f, Seq(b))
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.size.toFloat == 10f, s"${quad.size} should be 10f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Fork with 4 empty quadrants") {
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.size.toFloat == 10f, s"${quad.size} should be 10f")
    assert(quad.mass ~= 0f, s"${quad.mass} should be 0f")
    assert(quad.massX ~= 20f, s"${quad.massX} should be 20f")
    assert(quad.massY ~= 30f, s"${quad.massY} should be 30f")
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  test("Fork with 4 empty quadrants add to nw") {
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.insert(new Body(100f, 17.5f, 27.5f, 1f, 1f)).nw.isInstanceOf[Leaf])
  }

  test("Fork with 4 empty quadrants add to ne") {
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.insert(new Body(100f, 22.5f, 27.5f, 1f, 1f)).ne.isInstanceOf[Leaf])
  }

  test("Fork with 4 empty quadrants add to sw") {
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.insert(new Body(100f, 17.5f, 32.5f, 1f, 1f)).sw.isInstanceOf[Leaf])
  }

  test("Fork with 4 empty quadrants add to se") {
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.insert(new Body(100f, 22.5f, 32.5f, 1f, 1f)).se.isInstanceOf[Leaf])
  }

  test("Empty.insert(b) should return a Leaf with only that body") {
    val quad = Empty(51f, 46.3f, 5f)
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"$centerX should be 51f")
        assert(centerY == 46.3f, s"$centerY should be 46.3f")
        assert(size == 5f, s"$size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

  test("Fork.insert(b) should return a Leaf with two bodies in nw") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad =Fork(nw, ne, sw, se)

    val inserted = quad.insert(new Body(124f, 19f, 26f, 0f, 0f))
    inserted match {
      case Fork(nw, ne, sw, se) =>
        assert(inserted.total === nw.total)
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

  test("Fork.insert(b) with coordinates > abs(1800)") {
    val b = new Body(0f, -1828.7684f, -1545.5160f, 0f, 0f)
    val nw = Leaf(-1782.908f, -1496.5632f, 152.73679f, Seq(b))
    val ne = Empty(-1630.1711f, -1496.5632f, 152.73679f)
    val sw = Empty(-1782.908f, -1343.8264f, 152.73679f)
    val se = Empty(-1630.1711f, -1343.8264f, 152.73679f)
    val quad = Fork(nw, ne, sw, se)

    val inserted = quad.insert(new Body(0f, -1828.7686f, -1545.5162f, 0f, 0f))
    inserted match {
      case Fork(nw, ne, sw, se) =>
        assert(inserted.total === nw.total)
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

  test("Fork.insert(b) tight boundaries with coordinates > abs(1800)") {
    val b = new Body(0f, -1828.7686f, -1545.5162f, 0f, 0f)
    val nw = Leaf(-1828.7684f, -1545.5165f, 0.0011652892f, Seq(b))
    val ne = Empty(-1828.7672f, -1545.5165f, 0.0011652892f)
    val sw = Empty(-1828.7684f, -1545.5153f, 0.0011652892f)
    val se = Empty(-1828.7672f, -1545.5153f, 0.0011652892f)
    val quad = Fork(nw, ne, sw, se)

    val inserted = quad.insert(new Body(0f, -1828.7684f, -1545.516f, 0f, 0f))
    inserted match {
      case Fork(nw, ne, sw, se) =>
        assert(inserted.total === nw.total)
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

  test("Leaf with 1 body") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))

    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.size ~= 5f, s"${quad.size} should be 5f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Leaf with 1 body and above minimum size should create fork") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, minimumSize + 0.000001f, Seq(b1))

    //Make sure we are above the minimum size
    assert(quad.size ~= minimumSize + 0.000001f)

    val b2 = new Body(124f, 18f, 26.00001f, 0f, 0f)
    val newQuad = quad.insert(b2)
    assert(newQuad.isInstanceOf[Fork])
  }

  // test cases for Body

  test("Body.updated should do nothing for Empty quad trees") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    assert(body.xspeed == 0f)
    assert(body.yspeed == 0f)
  }

  test("Body.updated should take bodies in a Leaf into account") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    assert(body.xspeed ~= 12.587037f)
    assert(body.yspeed ~= 0.015557117f)
  }

  test("Body.updated should take distant fork into account") {
    val b1 = new Body(123f, 1800f, 2600f, 0f, 0f)

    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad =Fork(nw, ne, sw, se)

    val body = b1.updated(quad)

    assert(body.mass == 123)
    assert(body.xspeed ~= -7.1434E-6)
    assert(body.yspeed ~= -1.0318E-5)
    assert(body.x == 1800)
    assert(body.y == 2600)
  }

  test("Body.updated should recurse on close quad") {
    val b1 = new Body(123f, 20.1f, 30.5f, 0f, 0f)
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad =Fork(nw, ne, sw, se)

    val body = b1.updated(quad)

    assert(body.mass == 123)
    assert(body.xspeed ~= -2.1092827)
    assert(body.yspeed ~= -4.519891)
    assert(body.x ~= 20.1f)
    assert(body.y ~= 30.5f)
  }

  // test cases for sector matrix

  test("2 x 2 SectorMatirx index should compute the proper matrix index") {
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 6
    boundaries.maxY = 6
    val sectorMatrix = new SectorMatrix(boundaries, 3)

    assert(sectorMatrix.matrixIndex(0, 0) == 0)
    assert(sectorMatrix.matrixIndex(2.665f, 0) == 0)
    assert(sectorMatrix.matrixIndex(2.667f, 0) == 1)
    assert(sectorMatrix.matrixIndex(4.332f, 0) == 1)
    assert(sectorMatrix.matrixIndex(4.334f, 0) == 2)
    assert(sectorMatrix.matrixIndex(5.99f, 0) == 2)
    assert(sectorMatrix.matrixIndex(7.99f, 0) == 2)

    assert(sectorMatrix.matrixIndex(0, 2.665f) == 0)
    assert(sectorMatrix.matrixIndex(0, 2.667f) == 3)
    assert(sectorMatrix.matrixIndex(5.99f, 5.99f) == 8)
  }

  test("8 x 8 SectorMatirx index should compute the proper matrix index") {
    val sectorMatrix = genSectorMatrix(1, 1, 97, 97, 8)

    assert(sectorMatrix.matrixIndex(1, 1) == 0)
    assert(sectorMatrix.matrixIndex(0.9F, 0.9F) == 0)

    //Test x boundaries
    assert(sectorMatrix.matrixIndex(12.999f, 1) == 0)
    assert(sectorMatrix.matrixIndex(13f, 1) == 1)
    assert(sectorMatrix.matrixIndex(24.999f, 1) == 1)
    assert(sectorMatrix.matrixIndex(25f, 1) == 2)
    assert(sectorMatrix.matrixIndex(84.999f, 1) == 6)
    assert(sectorMatrix.matrixIndex(85f, 1) == 7)
    assert(sectorMatrix.matrixIndex(95.99f, 1) == 7)
    assert(sectorMatrix.matrixIndex(96f, 1) == 7)
    assert(sectorMatrix.matrixIndex(96.1f, 1) == 7)
  }

  test("'SectorMatrix.+=' should add a body at (2,2) to the correct bucket of a sector matrix of size 96") {
    val body = new Body(5, 2, 2, 0.1f, 0.1f)
    val sm = genSectorMatrix(1, 1, 97, 97, 8)
    sm += body
    val res = sm(0, 0).size == 1 && sm(0, 0).exists(_ == body)
    assert(res, s"Body not found in the right sector")
  }

  test("'SectorMatrix.+=' should add a body at (25, 47) to the correct bucket of a sector matrix of size 96") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val sm = genSectorMatrix(1, 1, 97, 97, 8)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).exists(_ == body)
    assert(res, s"Body not found in the right sector")
  }

  test("'SectorMatrix.combine should add a body at (25,47) to the correct bucket of a sector matrix of size 96") {
    val body1 = new Body(5, 2, 2, 0.1f, 0.1f)
    val sm1 = genSectorMatrix(1, 1, 97, 97, SECTOR_PRECISION)
    sm1 += body1

    val res1 = sm1(0, 0).size == 1 && sm1(0, 0).exists(_ == body1)
    assert(res1, s"Body1 not found in the right sector")

    val body2 = new Body(6, 35, 1, 0.1f, 0.1f)
    val sm2 = genSectorMatrix(1, 1, 97, 97, SECTOR_PRECISION)
    sm2 += body2
    val body3 = new Body(6, 35, 2, 0.1f, 0.1f)
    sm2 += body3

    val res2 = sm2(2, 0).size == 2 && sm2(2, 0).exists(_ == body2) && sm2(2, 0).exists(_ == body3)
    assert(res2, s"Body2 not found in the right sector")

    val smCombined = sm1.combine(sm2)
    val res3 = smCombined(0, 0).size == 1 && smCombined(0, 0).exists(_ == body1) &&
      smCombined(2, 0).exists(_ == body2) && smCombined(2, 0).exists(_ == body3)
    assert(res3, s"Body1, Body2 or Body3 not found in the right sector")
  }

  test("'SectorMatrix.combine' should correctly combine two sector matrices of size 96 that contain some points ") {
    val body1 = new Body(5, 2, 2, 0.1f, 0.1f)
    val sm1 = genSectorMatrix(1, 1, 97, 97, SECTOR_PRECISION)
    sm1 += body1

    val body2 = new Body(6, 12, 72, 0.1f, 0.1f)
    sm1 += body2

    val res1 = sm1(0, 0).size == 1 && sm1(0, 0).exists(_ == body1) &&
      sm1(0, 5).size == 1 && sm1(0, 5).exists(_ == body2)
    assert(res1, s"Body 1 or Body2 not found in the right sector")

    val body3 = new Body(6, 12.1f, 72, 0.1f, 0.1f)
    val sm2 = genSectorMatrix(1, 1, 97, 97, SECTOR_PRECISION)
    sm2 += body3

    val body4 = new Body(6, 12.2f, 72, 0.1f, 0.1f)
    sm2 += body4

    val res2 = sm2(0, 5).size == 2 && sm2(0, 5).exists(_ == body3) && sm2(0, 5).exists(_ == body4)
    assert(res2, s"Body2 not found in the right sector")

    val smCombined = sm1.combine(sm2)
    val res3 = smCombined(0, 0).size == 1 && smCombined(0, 0).exists(_ == body1) &&
      smCombined(0, 5).size == 3 &&
      smCombined(0, 5).exists(_ == body2) && smCombined(0, 5).exists(_ == body3) && smCombined(0, 5).exists(_ == body4)
    assert(res3, s"Body1, Body2, Body3 or Body4 not found in the right sector")
  }

  test("'computeSectorMatrix combine should add a body at (25,47) to the correct bucket of a sector matrix of size 96") {
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.maxX = 97
    boundaries.minY = 1
    boundaries.maxY = 97

    val body1 = new Body(5, 25, 47, 0.1f, 0.1f)
    val body2 = new Body(5, 35, 1, 0.1f, 0.1f)
    val body3 = new Body(5, 35.1f, 2, 0.1f, 0.1f)
    val body4 = new Body(5, 45.9f, 2, 0.1f, 0.1f)

    val sm = new Simulator(null, new TimeStatistics).computeSectorMatrix(Seq(body1, body2, body3, body4), boundaries)
    assert(sm(2, 3).size == 1 && sm(2, 3).exists(_ == body1), "Body1 not found in the correct sector")
    assert(sm(2, 0).size == 2 && sm(2, 0).exists(_ == body2), "Body2 not found in the correct sector")
    assert(sm(2, 0).exists(_ == body3), "Body3 not found in the correct sector")
    assert(sm(3, 0).size == 1 && sm(3, 0).exists(_ == body4), "Body4 not found in the correct sector")
  }
}

object FloatOps {
  private val precisionThreshold = 1e-4

  /** Floating comparison: assert(float ~= 1.7f). */
  implicit class FloatOps(val self: Float) extends AnyVal {
    def ~=(that: Float): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Long floating comparison: assert(double ~= 1.7). */
  implicit class DoubleOps(val self: Double) extends AnyVal {
    def ~=(that: Double): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Floating sequences comparison: assert(floatSeq ~= Seq(0.5f, 1.7f). */
  implicit class FloatSequenceOps(val self: Seq[Float]) extends AnyVal {
    def ~=(that: Seq[Float]): Boolean =
      self.size == that.size &&
        self.zip(that).forall { case (a, b) =>
          abs(a - b) < precisionThreshold
        }
  }
}

