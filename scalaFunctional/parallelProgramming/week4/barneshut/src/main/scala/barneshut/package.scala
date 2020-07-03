import common._
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue
    var minY = Float.MaxValue
    var maxX = Float.MinValue
    var maxY = Float.MinValue

    def width = maxX - minX
    def height = maxY - minY
    def size = math.max(width, height)
    def centerX = minX + width / 2
    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float
    def massY: Float
    def mass: Float
    def centerX: Float
    def centerY: Float
    def size: Float
    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0
    def total: Int = 0

    def insert(b: Body): Quad = {
      Leaf(centerX, centerY, size, Seq(b))
    }
  }

  case class Fork(nw: Quad, ne: Quad, sw: Quad, se: Quad) extends Quad {

    private val seqOfQuads = Seq[Quad](nw,ne,sw,se)
    val size: Float = nw.size * 2.0f
    val centerX: Float = nw.centerX + nw.size / 2.0f
    val centerY: Float = nw.centerY + nw.size / 2.0f
    val mass: Float = seqOfQuads.foldLeft(0f)((acc, q) => acc + q.mass)
    val massX: Float = mass match {
      case 0 => centerX
      case _ => seqOfQuads.foldLeft(0f)((a, b) => a + b.massX * b.mass) / mass
    }

    val massY: Float = mass match {
      case 0 => centerY
      case _ => seqOfQuads.foldLeft(0f)((a, b) => a + b.massY * b.mass) / mass
    }
    val total: Int = seqOfQuads.foldLeft(0)((acc, q) => acc + q.total)

    def insert(b: Body): Fork = {
      val nwD = distance(b.x, b.y, nw.centerX, nw.centerY)
      val neD = distance(b.x, b.y, ne.centerX, ne.centerY)
      val swD = distance(b.x, b.y, sw.centerX, sw.centerY)
      val seD = distance(b.x, b.y, se.centerX, se.centerY)

      val distances = Seq(nwD, neD, swD, seD)
      if(nwD == distances.min)
        Fork(nw.insert(b), ne, sw, se)
      else if(neD == distances.min)
        Fork(nw, ne.insert(b), sw, se)
      else if(swD == distances.min)
        Fork(nw, ne, sw.insert(b), se)
      else
        Fork(nw, ne, sw, se.insert(b))
    }
  }

  case class Leaf(centerX: Float,
                  centerY: Float,
                  size: Float,
                  bodies: Seq[Body]) extends Quad {
    def accumFn(a: (Float, Float, Float), body: Body): (Float, Float, Float) = {
      (a._1 + body.mass,
       a._2 + (body.mass * body.x),
       a._3 + (body.mass * body.y))
    }

    def combOp(a: (Float, Float, Float),
               b: (Float, Float, Float)): (Float, Float, Float) = {
      (a._1 + b._1, a._2 + b._2, a._3 + b._3)
    }

    def computeValues: (Float, Float, Float) = {
      val theSums = bodies.aggregate((0.0f, 0.0f, 0.0f))(accumFn, combOp)
      (theSums._1, theSums._2 / theSums._1, theSums._3 / theSums._1)
    }

    val (mass, massX, massY) = computeValues
    val total: Int = bodies.size

    def insertBodyIntoFork(fork:Fork, b: Body, bodies: Seq[Body]) : Fork = {
      if(bodies.isEmpty)
        fork.insert(b)
      else {
        val inserted = fork.insert(b)
        insertBodyIntoFork(inserted, bodies.head, bodies.tail)
      }
    }

    def insert(b: Body): Quad = {
//      println(s"quad insert(${b.x}, ${b.y})")
      if(size > minimumSize) {
        val newSize = size / 2.0f
        val newSubCellSize = newSize / 2.0f
        val wCenterX = centerX - newSubCellSize
        val eCenterX = centerX + newSubCellSize
        val nCenterY = centerY - newSubCellSize
        val sCenterY = centerY + newSubCellSize

        val nw = Empty(wCenterX, nCenterY, newSize)
        val ne = Empty(eCenterX, nCenterY, newSize)
        val sw = Empty(wCenterX, sCenterY, newSize)
        val se = Empty(eCenterX, sCenterY, newSize)

        insertBodyIntoFork(Fork(nw, ne, sw, se), b, bodies)
      } else {
        val leaf = Leaf(centerX, centerY, size, b +: bodies)
        leaf
      }
    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float =
    gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float,
             val x: Float,
             val y: Float,
             val xspeed: Float,
             val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
        // no force
        case Leaf(_, _, _, bodies) =>
          bodies.foreach(x => addForce(x.mass, x.x, x.y))
        // add force contribution of each body by calling addForce
        case Fork(nw, ne, sw, se) =>
        // see if node is far enough from the body,
        // or recursion is needed
          val dist = distance(x, quad.massX, y, quad.massY)
          if((quad.size / dist) < theta) {
            traverse(nw)
            traverse(ne)
            traverse(sw)
            traverse(se)
          }
          else {
            addForce(quad.mass, quad.massX, quad.massY)
          }
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)

    def matrixIndex(x:Float, y:Float) : Int = {
      val matX =
        if(x <= boundaries.minX) {
          0
        } else if (x >= boundaries.maxX) {
          (boundaries.width / sectorSize) - 1
        } else {
          (x - boundaries.minX) / sectorSize
        }

      val matY =
        if(y <= boundaries.minY) {
          0
        } else if (y >= boundaries.maxY) {
          (boundaries.height / sectorSize) - 1
        } else {
          (y - boundaries.minY) / sectorSize
        }

      val matixIndex = matX.toInt  + (matY.toInt * sectorPrecision)
      matixIndex.toInt
    }

    for (i <- matrix.indices) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      val mi = matrixIndex(b.x, b.y)
      matrix(mi) += b
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      val retSectorMatrix =  new SectorMatrix(boundaries, sectorPrecision)
      for(i <- matrix.indices) {
        retSectorMatrix.matrix(i) = this.matrix(i).combine(that.matrix(i))
      }
      retSectorMatrix
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR)
              parallel(
                quad(x, y, nspan, nAchievedParallelism),
                quad(x + nspan, y, nspan, nAchievedParallelism),
                quad(x, y + nspan, nspan, nAchievedParallelism),
                quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
              )
            else
              (
                quad(x, y, nspan, nAchievedParallelism),
                quad(x + nspan, y, nspan, nAchievedParallelism),
                quad(x, y + nspan, nspan, nAchievedParallelism),
                quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
              )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: => T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None               => timeMap(title) = (0.0, 0)
      }

      println(
        s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) =>
          k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString ("\n")
    }
  }
}
