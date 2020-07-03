package scalashop

import org.scalameter._
import common._

object HorizontalBoxBlurRunner {

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
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for (rowIdx <- from until end; colIdx <- 0 until src.width) {
      val cell = boxBlurKernel(src, colIdx, rowIdx, radius)
//      println(s"dst($colIdx, $rowIdx)=$cell")
      dst(colIdx, rowIdx) = cell
    }
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
//    println(s"----------> parBlur numTasks = $numTasks")

    val numRowsPerTask = if(src.height >= numTasks) {
      val rowsModulo = src.height % numTasks
//      println(s"----------> rowsModulo = $rowsModulo")
      if (rowsModulo == 0) src.height / numTasks else rowsModulo
    } else
      1

    val tasks = for {
      startRow <- 0 until src.height by numRowsPerTask
      endRow = startRow + numRowsPerTask

    } yield {
      if(endRow <= src.height) {
//        println(s"endRow <= src.height numRowsPerTask = $numRowsPerTask -->task($startRow, $endRow)")
        task(blur(src, dst, startRow, endRow, radius))
      } else {
//        println(s"endRow > src.height numRowsPerTask = $numRowsPerTask -->task($startRow, ${src.height})")
        task(blur(src, dst, startRow, src.height, radius))
      }
    }

    tasks foreach {x=> x.join()}
  }

}
