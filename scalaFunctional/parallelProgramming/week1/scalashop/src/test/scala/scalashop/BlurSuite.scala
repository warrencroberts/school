package scalashop

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BlurSuite extends FunSuite {

  def check(dst: Img)(x: Int, y: Int, expected: Int) = assert(dst(x, y) == expected, s"(destination($x, $y) should be $expected)")

  test("boxBlurKernel should correctly handle radius 0") {
    val src = new Img(5, 5)

    for (x <- 0 until 5; y <- 0 until 5)
      src(x, y) = rgba(x, y, x + y, math.abs(x - y))

    for (x <- 0 until 5; y <- 0 until 5)
      assert(boxBlurKernel(src, x, y, 0) === rgba(x, y, x + y, math.abs(x - y)),
        "boxBlurKernel(_,_,0) should be identity.")
  }

  test("boxBlurKernel should return the correct value on an interior pixel " +
    "of a 3x4 image with radius 1") {
    val src = new Img(3, 4)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8
    src(0, 3) = 50; src(1, 3) = 11; src(2, 3) = 16

    assert(boxBlurKernel(src, 1, 2, 1) === 12,
      s"(boxBlurKernel(1, 2, 1) should be 12, " +
        s"but it's ${boxBlurKernel(src, 1, 2, 1)})")
  }

  test("boxBlurKernel should compute the averages of red, blue, green and alpha channels separately  " +
    "of a 3x3 image with radius 1 on left edge") {
    val src = new Img(3, 3)
    src(0, 0) = 0xBFCFDFEF; src(1, 0) = 0xBFCFDFEF; src(2, 0) = 0x30303030
    src(0, 1) = 0xBFCFDFEF; src(1, 1) = 0xBFCFDFEF; src(2, 1) = 0x30303030
    src(0, 2) = 0xBFCFDFEF; src(1, 2) = 0xBFCFDFEF; src(2, 2) = 0x30303030

    var col = 0
    var row = 0
    var testCell = boxBlurKernel(src, col, row, 1)
    assert(red(testCell) === 0xBF, s"(red)")
    assert(green(testCell) === 0xCF, s"(green)")
    assert(blue(testCell) === 0xDF, s"(blue)")
    assert(alpha(testCell) === 0xEF, s"(alpha)")

    col = 0
    row = 1
    testCell = boxBlurKernel(src, col, row, 1)
    assert(red(testCell) === 0xBF, s"(red)")
    assert(green(testCell) === 0xCF, s"(green)")
    assert(blue(testCell) === 0xDF, s"(blue)")
    assert(alpha(testCell) === 0xEF, s"(alpha)")

    col = 0
    row = 2
    testCell = boxBlurKernel(src, col, row, 1)
    assert(red(testCell) === 0xBF, s"(red)")
    assert(green(testCell) === 0xCF, s"(green)")
    assert(blue(testCell) === 0xDF, s"(blue)")
    assert(alpha(testCell) === 0xEF, s"(alpha)")
  }

  test("HorizontalBoxBlur.blur with radius 1 should correctly blur the entire 3x3 image") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    val radius = 1
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8

    def checkEm(testDest: Img) = {
      val chk = check(testDest)_

      chk(0, 0, 2)
      chk(0, 1, 3)
      chk(0, 2, 5)
      chk(1, 0, 2)
      chk(1, 1, 4)
      chk(1, 2, 5)
      chk(2, 0, 3)
      chk(2, 1, 4)
      chk(2, 2, 6)
    }

    HorizontalBoxBlur.blur(src, dst, 0, 3, radius)
    checkEm(dst)

    val dst1 = new Img(w, h)
    HorizontalBoxBlur.parBlur(src, dst1, 2, radius)
    checkEm(dst1)

  }

  def src5by5() : Img = {
    val w = 5
    val h = 5

    val src = new Img(w, h)
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2; src(3,0) = 3; src(4,0) = 4
    src(0, 1) = 5; src(1, 1) = 6; src(2, 1) = 7; src(3,1) = 8; src(4,1) = 9
    src(0, 2) = 10; src(1, 2) = 11; src(2, 2) = 12; src(3,2) = 13;  src(4,2) = 14
    src(0, 3) = 15; src(1, 3) = 16; src(2, 3) = 17; src(3,3) = 18; src(4,3) = 19
    src(0,4) = 20;  src(1,4) = 21;  src(2,4) = 22;  src(3,4) =23; src(4,4) = 24
    src
  }

  def checkEm(testDest: Img) = {
    val chk = check(testDest) _

    chk(0, 0, 3)
    chk(1, 0, 3)
    chk(2, 0, 4)
    chk(3, 0, 5)
    chk(4, 0, 6)
    chk(0, 1, 5)
    chk(1, 1, 6)
    chk(2, 1, 7)
    chk(3, 1, 8)
    chk(4, 1, 8)
    chk(0, 2, 10)
    chk(1, 2, 11)
    chk(2, 2, 12)
    chk(3, 2, 13)
    chk(4, 2, 13)
    chk(0, 3, 15)
    chk(1, 3, 16)
    chk(2, 3, 17)
    chk(3, 3, 18)
    chk(4, 3, 18)
    chk(0, 4, 18)
    chk(1, 4, 18)
    chk(2, 4, 19)
    chk(3, 4, 20)
    chk(4, 4, 21)
  }

  test("HorizontalBoxBlur.parBlur with radius 1 should correctly blur the entire 5x5 image") {
    val src = src5by5()

    val w = 5
    val h = 5
    val dst = new Img(w, h)
    val radius = 1

    HorizontalBoxBlur.blur(src, dst, 0, 5, radius)
    checkEm(dst)

    val dst1 = new Img(w, h)
    HorizontalBoxBlur.parBlur(src, dst1, 1, radius)
    checkEm(dst1)

    val dst2 = new Img(w, h)
    HorizontalBoxBlur.parBlur(src, dst2, 2, radius)
    checkEm(dst2)

    val dst3 = new Img(w, h)
    HorizontalBoxBlur.parBlur(src, dst3, 3, radius)
    checkEm(dst3)

    val dst4 = new Img(w, h)
    HorizontalBoxBlur.parBlur(src, dst4, 4, radius)
    checkEm(dst4)

    val dst5 = new Img(w, h)
    HorizontalBoxBlur.parBlur(src, dst5, 5, radius)
    checkEm(dst5)

    val dst6 = new Img(w, h)
    HorizontalBoxBlur.parBlur(src, dst6, 6, radius)
    checkEm(dst6)
  }

  test("VerticalBoxBlur.parBlur with radius 1 should correctly blur the entire 5x5 image") {
    val src = src5by5()

    val w = 5
    val h = 5
    val dst = new Img(w, h)
    val radius = 1

    VerticalBoxBlur.blur(src, dst, 0, 5, radius)
    checkEm(dst)

    val dst1 = new Img(w, h)
    VerticalBoxBlur.parBlur(src, dst1, 1, radius)
    checkEm(dst1)

    val dst2 = new Img(w, h)
    VerticalBoxBlur.parBlur(src, dst2, 2, radius)
    checkEm(dst2)

    val dst3 = new Img(w, h)
    VerticalBoxBlur.parBlur(src, dst3, 3, radius)
    checkEm(dst3)

    val dst4 = new Img(w, h)
    VerticalBoxBlur.parBlur(src, dst4, 4, radius)
    checkEm(dst4)

    val dst5 = new Img(w, h)
    VerticalBoxBlur.parBlur(src, dst5, 5, radius)
    checkEm(dst5)

    val dst6 = new Img(w, h)
    VerticalBoxBlur.parBlur(src, dst6, 6, radius)
    checkEm(dst6)
  }
}
