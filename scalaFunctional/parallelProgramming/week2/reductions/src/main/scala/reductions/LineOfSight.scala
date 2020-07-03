package reductions

import java.util.concurrent.atomic.AtomicInteger

import org.scalameter._
import common._

object LineOfSightRunner {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {

  def max(a: Float, b: Float): Float = if (a > b) a else b

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    var maxSoFar = 0f
    output(0)
    for (i <- 1 until input.length) {
      val x = input(i) / i
      maxSoFar = max(maxSoFar, x)
      output(i) = maxSoFar
    }
  }

  sealed abstract class Tree {
    def maxPrevious: Float
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious = max(left.maxPrevious, right.maxPrevious)
    override def toString = {
      s"Node($maxPrevious)" +
        s"..l..$left" +
        s"..r..$right\n"
    }
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree {
    override def toString = {
      s"...Leaf($from, $until, $maxPrevious)"
    }
  }

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, to: Int): Float = {
    var maxSoFar = 0f
    for(i <- from until to) {
      val inp = input(i)
      val x =  if(i > 0) inp / i else 0
      maxSoFar = max(maxSoFar, x)
    }

    maxSoFar
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, to: Int, threshold: Int): Tree = {
    val delta = to - from
//    println(s"upsweep($from, $to, $delta)")
    if(delta <= threshold) {
      Leaf(from, to, upsweepSequential(input, from, to))
    } else {
      val mid = from + delta/2
      val (tL, tR) = parallel(upsweep(input, from, mid, threshold), upsweep(input, mid, to, threshold))
      Node(tL, tR)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float], startingAngle: Float, from: Int, to: Int): Unit = {
//    println(s"downsweepSequential ($startingAngle, $from, $to)")

    if(from < to) {
      var a = startingAngle
      for(i <- from until to) {
        val inp = input(i)
        val newSlope = if(i > 0) inp / i else 0
        a = max(a, newSlope)
        output(i) =  a
      }
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepTraverse` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float, tree: Tree): Unit = {
    tree match {
      case Leaf(from, to, _) =>
        downsweepSequential(input, output, startingAngle, from, to)
      case Node(l, r) => parallel(
        downsweep(input, output, startingAngle, l), downsweep(input, output, max(startingAngle, l.maxPrevious), r))
    }
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float], threshold: Int): Unit = {
    val t = upsweep(input, 0, input.length, threshold)
//    println(s"parLineOfSight : \n$t")
    downsweep(input, output, 0, t)
  }
}
