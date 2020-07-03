package streams

import common._

/**
 * This component implements the solver for the Bloxorz game
 */
trait Solver extends GameDef {

  /**
   * Returns `true` if the block `b` is at the final position
   */
  def done(b: Block): Boolean = b.b1 == goal && b.b2 == goal

  /**
   * This function takes two arguments: the current block `b` and
   * a list of moves `history` that was required to reach the
   * position of `b`.
   *
   * The `head` element of the `history` list is the latest move
   * that was executed, i.e. the last move that was performed for
   * the block to end up at position `b`.
   *
   * The function returns a stream of pairs: the first element of
   * the each pair is a neighboring block, and the second element
   * is the augmented history of moves required to reach this block.
   *
   * It should only return valid neighbors, i.e. block positions
   * that are inside the terrain.
   */
  def neighborsWithHistory(b: Block, history: Moves): Stream[(Block, Moves)] = {
    val historyTuples = for {
      (theNeighbor, theMove) <- b.legalNeighbors
    } yield
      (theNeighbor, theMove :: history)

    historyTuples.toStream
  }


  /**
   * This function returns the list of neighbors without the block
   * positions that have already been explored. We will use it to
   * make sure that we don't explore circular paths.
   */
  def newNeighborsOnly(neighbors: Stream[(Block, Moves)],
                       explored: Set[Block]): Stream[(Block, Moves)] = {
    neighbors.filter((x) =>
      !explored.contains(x._1))
  }

  /**
    * The function `from` returns the stream of all possible paths
    * that can be followed, starting at the `head` of the `initial`
    * stream.
    *
    * The blocks in the stream `initial` are sorted by ascending path
    * length: the block positions with the shortest paths (length of
    * move list) are at the head of the stream.
    *
    * The parameter `explored` is a set of block positions that have
    * been visited before, on the path to any of the blocks in the
    * stream `initial`. When search reaches a block that has already
    * been explored before, that position should not be included a
    * second time to avoid cycles.
    *
    * The resulting stream should be sorted by ascending path length,
    * i.e. the block positions that can be reached with the fewest
    * amount of moves should appear first in the stream.
    *
    * Note: the solution should not look at or compare the lengths
    * of different paths - the implementation should naturally
    * construct the correctly sorted stream.
    */
  def from(initial: Stream[(Block, Moves)], explored: Explored): Stream[(Block, Moves)] = {

    def pullNewNeighbors(currBlockAndMoves: (Block, Moves), explored: Explored) : (Stream[(Block, Moves)], Explored) = {
      val neighbors = neighborsWithHistory(currBlockAndMoves._1, currBlockAndMoves._2)
      val newNeighbors = newNeighborsOnly(neighbors, explored)
      val newExplored = newNeighbors.foldLeft(explored)((b, n) => b + n._1)
      (newNeighbors, newExplored)
    }

    def depthFirst(workQueue: Stream[(Block, Moves)], explored: Explored): Stream[(Block, Moves)] = {
      if (workQueue.isEmpty)
        Stream.empty
      else {
        val nextElement = for {
          head <- workQueue
          nextExploration = pullNewNeighbors(head, explored)
        }
          yield nextExploration

        workQueue #::: depthFirst(nextElement.head._1, nextElement.head._2)
      }
    }

    def breadthFirst(workQueue: Stream[(Block, Moves)], explored: Explored): Stream[(Block, Moves)] = {
      if (workQueue.isEmpty)
        Stream.empty
      else {
        val head = workQueue.head
        val tail = workQueue.tail
        val nextExploration = pullNewNeighbors(head, explored)

        // return head of initial list.  add new neighbors to tail of the work queue
        head #:: breadthFirst(tail ++ nextExploration._1, explored ++ nextExploration._2)
      }
    }

//    depthFirst(initial, explored)
    breadthFirst(initial, explored)
  }

  /**
   * The stream of all paths that begin at the starting block.
   */
  lazy val pathsFromStart: Stream[(Block, Moves)] = from(Stream((startBlock, List())), Set[Block]())

  /**
   * Returns a stream of all possible pairs of the goal block along
   * with the history how it was reached.
   */
  lazy val pathsToGoal: Stream[(Block, Moves)] = pathsFromStart.filter(x =>
    x._1 == goalBlock)

  /**
   * The (or one of the) shortest sequence(s) of moves to reach the
   * goal. If the goal cannot be reached, the empty list is returned.
   *
   * Note: the `head` element of the returned list should represent
   * the first move that the player should perform from the starting
   * position.
   */
  lazy val solution: Moves = pathsToGoal.head._2.reverse
}
