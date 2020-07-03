package water

import org.scalatest.FunSuite

/**
  * Created by wrober003c on 2/15/17.
  */
class PouringSuite extends FunSuite {
  test("pouring") {
    val problem = new Pouring(Vector(4, 7))

    problem.moves

    println(problem.pathSets.take(3).toList)

    problem.solution(6)
  }

}
