package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance1(chars: Array[Char]): Boolean = {
    def sawOpen(chars: Array[Char], openCount: Int): Boolean = {
      if (chars.isEmpty)
        openCount == 0
      else if (openCount < 0) {
        false
      }
      else {
        val currChar = chars.head

        if (currChar == '(')
          sawOpen(chars.tail, openCount + 1)
        else if (currChar == ')')
          sawOpen(chars.tail, openCount - 1)
        else
          sawOpen(chars.tail, openCount)
      }
    }

    sawOpen(chars, 0)
  }

  def balance(chars: Array[Char]): Boolean = {
    def sawOpen(idx: Int, openCount: Int): Boolean = {
      if (idx >= chars.length)
        openCount == 0
      else if (openCount < 0) {
        false
      }
      else {
        val currChar = chars(idx)

        if (currChar == '(')
          sawOpen(idx + 1, openCount + 1)
        else if (currChar == ')')
          sawOpen(idx + 1, openCount - 1)
        else
          sawOpen(idx + 1, openCount)
      }
    }

    sawOpen(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  case class BranchCounts(var bal: Int, var unbal: Int) {
    override def toString: String = {
      s"BranchCounts($bal, $unbal)"
    }
  }

  case class ResultCounts(eastBound: BranchCounts, westBound: BranchCounts) {
    def isBalanced = (eastBound.unbal == 0) && (westBound.unbal == 0) && (eastBound.bal == westBound.bal)

    override def toString: String = {
      s"..ResultCounts: $eastBound, $westBound\n"
    }
  }

  object UtilFunctions {
    def leftFn(c: Char, counts: BranchCounts): Unit = c match {
      case '(' => counts.bal += 1
      case ')' =>
        if (counts.bal > 0)
          counts.bal -= 1
        else if (counts.bal == 0)
          counts.unbal += 1
      case _ =>
    }

    def rightFn(c: Char, counts: BranchCounts): Unit = c match {
      case ')' => counts.bal += 1
      case '(' =>
        if (counts.bal > 0)
          counts.bal -= 1
        else if (counts.bal == 0)
          counts.unbal += 1
        else
          counts
      case _ =>
    }

    val fn = (left: Int, right: Int, counts: (ResultCounts, ResultCounts)) => {
//      println(s"fn($left, $right)\n")

        val leftResults  = counts._1
        val rightResults = counts._2
//        println(s"fn($left, $right) mergeLBranchCounts\n$leftResults$rightResults")

        //If the left side has unbalanced start parens then new start_parens = left_starts + right_starts - unbal_right_closes
        // ((  + ( == (((  or (( + ) == (
        if (leftResults.eastBound.bal > 0) {

          // Determine balance difference
          val balanceDifference = leftResults.eastBound.bal + rightResults.eastBound.bal - rightResults.westBound.bal

          val retResults = ResultCounts(BranchCounts(balanceDifference, 0), BranchCounts(0, 0))

//          println(s"fn($left, $right) --> retResults:\n$retResults")
          retResults
        }
        else {
          if(rightResults.westBound.bal > 0) {
            val balanceDifference = leftResults.westBound.bal + rightResults.westBound.bal
            val retResults = ResultCounts(BranchCounts(0, balanceDifference), BranchCounts(balanceDifference, 0))

//            println(s"fn($left, $right) --> retResults:\n$retResults")
            retResults
          } else {

            val retResults = ResultCounts(BranchCounts(0, leftResults.eastBound.unbal), BranchCounts(0, rightResults.westBound.unbal))

//            println(s"fn($left, $right) --> retResults:\n$retResults")
            retResults
          }
        }
    }

  }

  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    def traverse(left: Int, right: Int, leftFn: (Char, BranchCounts) => Unit, rightFn: (Char, BranchCounts) => Unit): ResultCounts = {
      def helper(lIdx: Int, rIdx: Int, acc: ResultCounts): ResultCounts = {
//        println(s"helper($lIdx, $rIdx, $acc")
        leftFn(chars(lIdx), acc.eastBound)
        rightFn(chars(rIdx), acc.westBound)

        if (lIdx == right - 1) {
//          println(s"helper-result($lIdx, $rIdx, $acc")
          acc
        } else {
          helper(lIdx + 1, rIdx - 1, acc)
        }
      }

      helper(left, right - 1, ResultCounts(BranchCounts(0, 0), BranchCounts(0, 0)))
    }

    def reduce(left: Int, right: Int, side: String = ""): ResultCounts = {

//      println(s"...$side..reduce($left, $right)")
      if (right == 0)
        ResultCounts(BranchCounts(0, 0), BranchCounts(0, 0))
      else if ((right - left) <= threshold) {
        val retVal = traverse(left, right, UtilFunctions.leftFn, UtilFunctions.rightFn)
        retVal
      } else {
        val mid = left + (right - left) / 2
        val result = parallel(reduce(left, mid, "l"), reduce(mid, right, "r"))
        UtilFunctions.fn(left, right, result)
      }

    }

//    println(s"starting run (0,${chars.length})")
    val result = reduce(0, chars.length, "s")
    result.isBalanced
  }
}
