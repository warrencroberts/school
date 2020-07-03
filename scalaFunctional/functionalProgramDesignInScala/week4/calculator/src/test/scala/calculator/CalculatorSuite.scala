package calculator

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import TweetLength.MaxTweetLength
import calculator.Calculator.eval

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }

  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("calculator eval Literal") {

    val namedExpressions: Map[String, Signal[Expr]] =
      Map("a" ->  Signal(Literal(5.0)))

    assert(eval(Ref("a"), namedExpressions) === 5.0)

  }

  test("calculator eval Plus") {
    val namedExpressions: Map[String, Signal[Expr]] =
      Map("c" ->  Signal(Literal(5.0)),
        "d" -> Signal(Literal(5.0)))

    assert(eval(Plus(Ref("c"), Ref("d")), namedExpressions) === 10.0)
  }

    test("calculator eval Minus") {
    val namedExpressions: Map[String, Signal[Expr]] =
      Map("c" ->  Signal(Literal(5.0)),
        "d" -> Signal(Literal(5.0)))

    assert(eval(Minus(Ref("c"), Ref("d")), namedExpressions) === 0.0)

  }

  test("calculator eval Times") {
    val namedExpressions: Map[String, Signal[Expr]] =
      Map("c" ->  Signal(Literal(5.0)),
        "d" -> Signal(Literal(5.0)))

    assert(eval(Times(Ref("c"), Ref("d")), namedExpressions) === 25.0)

  }

  test("calculator eval Divide") {
    val namedExpressions: Map[String, Signal[Expr]] =
      Map("c" -> Signal(Literal(5.0)),
          "d" -> Signal(Literal(5.0)))

    assert(eval(Divide(Ref("c"), Ref("d")), namedExpressions) === 1.0)

  }

  test("calculator eval Plus Plus") {
    val namedExpressions: Map[String, Signal[Expr]] =
      Map("a" -> Signal(Literal(5.0)),
          "b" -> Signal(Literal(6.0)),
          "c" -> Signal(Literal(7.0)),
          "d" -> Signal(Literal(8.0)))

    assert(eval(Plus(Plus(Ref("a"), Ref("b")), Plus(Ref("c"), Ref("d"))), namedExpressions) === 26.0)

  }

}
