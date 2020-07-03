package streams

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: Moves): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)

    def isLengthIncreasing(x:Int, s: Stream[(Block, Moves)]) : Boolean = {
      if(s.isEmpty)
        true
      else {
        val currLength = s.head._2.length
        if(x <= currLength)
          isLengthIncreasing(currLength, s.tail)
        else
          false
      }
    }

  }


  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

  test("terrain test legal neighbors from start") {
    new Level1 {
      val neighbors: List[(Block, Move)] = startBlock.legalNeighbors
      assert(neighbors == List((startBlock.right, Right), (startBlock.down, Down)))
      assert(!neighbors.contains((startBlock.up, Up)))
      assert(!neighbors.contains((startBlock.left, Left)))
    }
  }

  test("terrain test legal neighbors from right of start") {
    new Level1 {
      val testBlock: Block = startBlock.right
      val neighbors: List[(Block, Move)] = testBlock.legalNeighbors
      assert(neighbors.contains((startBlock, Left)))
      assert(neighbors.contains((testBlock.right, Right)))
      assert(neighbors.contains((testBlock.down, Down)))
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("find goal char level 1") {
    new Level1 {
      assert(goal == Pos(4,7))
    }
  }

  test("neighborsWithHistory from startBlock") {
    new Level1 {
      val neighbors: List[(Block, Moves)] = neighborsWithHistory(startBlock, Nil).toList
      assert(neighbors.contains((startBlock.right, List(Right))))
      assert(neighbors.contains((startBlock.down, List(Down))))
      assert(!neighbors.contains((startBlock.left, List(Left))))
      assert(!neighbors.contains((startBlock.up, List(Up))))
    }
  }

  test("neighborsWithHistory from startBlock.right") {
    new Level1 {
      val testBlock: Block = startBlock.right
      val neighbors: List[(Block, Moves)] = neighborsWithHistory(testBlock, List(Right)).toList
      assert(neighbors.contains((testBlock.right, List(Right,Right))))
      assert(neighbors.contains((testBlock.left, List(Left,Right))))
      assert(neighbors.contains((testBlock.down, List(Down,Right))))
    }
  }

  test("newNeighborsOnly from startBlock.right.down") {
    new Level1 {
      val explored: Set[Block] = Set(startBlock, startBlock.right, startBlock.right.down)
      val testBlock: Block = startBlock.right.down
      val neighbors: Stream[(Block, Moves)] = neighborsWithHistory(testBlock, List(Down,Right))
      val newNeighbors: Stream[(Block, Moves)] = newNeighborsOnly(neighbors, explored)
      assert(newNeighbors.contains((testBlock.left, List(Left,Down,Right))))
      assert(newNeighbors.contains((testBlock.right, List(Right,Down,Right))))
      assert(newNeighbors.contains((testBlock.down, List(Down, Down,Right))))
      assert(!newNeighbors.contains((testBlock.up, List(Up,Down,Right))))
    }
  }

  test("the from function starting at goal.up.left") {
    new Level1 {
      val testBlock: Block = Block(goal, goal).up.left
      val explored: Set[Block] = Set(testBlock, testBlock.left, testBlock.left.left)

      val theFromStream: Stream[(Block, Moves)] = from(Stream((testBlock, List(Right,Right,Right))), explored)

      // Check for duplicate paths
      assert(theFromStream.length == theFromStream.toSet.size)

      // Verify that all path lengths are increasing.
      assert(isLengthIncreasing(0, theFromStream))
    }
  }

  test("the from function starting at start block") {
    new Level1 {
      // Check for duplicate paths
      assert(pathsFromStart.length == pathsFromStart.toSet.size)

      // Verify that all path lengths are increasing.
      assert(isLengthIncreasing(0, pathsFromStart))
    }
  }

  test("pathsToGoal") {
    new Level1 {
      val goalPaths: Stream[(Block, Moves)] = pathsToGoal

      assert(goalPaths.head._1 == goalBlock)
     }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

}
