package scalashop

import org.scalameter._
import common._

import scalashop.HorizontalBoxBlur.blur

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for (colIdx <- from until end; rowIdx <- 0 until src.height) {
      dst(colIdx, rowIdx) = boxBlurKernel(src, colIdx, rowIdx, radius)
    }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    //    println(s"----------> parBlur numTasks = $numTasks")

    val numColsPerTask = if(src.width >= numTasks) {
      val colsModule = src.width % numTasks
      //      println(s"----------> colsModule = colsModule")
      if (colsModule == 0) src.width / numTasks else colsModule
    } else
      1

    val tasks = for {
      startCol <- 0 until src.width by numColsPerTask
      endCol = startCol + numColsPerTask

    } yield {
      if(endCol <= src.width) {
        //        println(s"endCol <= src.width numColsPerTask = $numColsPerTask -->task($startCol, $endCol)")
        task(blur(src, dst, startCol, endCol, radius))
      } else {
        //        println(s"endCol > src.width numColsPerTask = $numColsPerTask -->task($startCol, ${src.height})")
        task(blur(src, dst, startCol, src.width, radius))
      }
    }

    tasks foreach {x=> x.join()}
  }

}
