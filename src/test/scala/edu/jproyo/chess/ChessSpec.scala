package edu.jproyo.chess.program

import org.scalatest._

class ChessSpec extends FlatSpec with Matchers {

  "Step 1 sample-moves.txt" should "nextMove forever and output moves" in {
    val sampleMoves = getClass.getClassLoader.getResource("data/sample-moves.txt")
    val chess = new Chess(sampleMoves.getPath)
    val expected = List(
      Move(Position(4,6),Position(4,4)),
      Move(Position(4,1),Position(4,3)),
      Move(Position(1,7),Position(2,5)),
      Move(Position(3,1),Position(3,2)),
      Move(Position(7,6),Position(7,5)),
      Move(Position(2,0),Position(4,2)),
      Move(Position(7,7),Position(7,6)))
    chess.runMoves shouldBe expected
  }

  "Step 1 sample-moves-invalid.txt" should "nextMove forever and output moves" in {
    val sampleMoves = getClass.getClassLoader.getResource("data/sample-moves-invalid.txt")
    val chess = new Chess(sampleMoves.getPath)
    val expected = List(
      Move(Position(4,6),Position(4,4)),
      Move(Position(4,1),Position(4,3)),
      Move(Position(1,7),Position(1,5)),
      Move(Position(3,1),Position(3,2)),
      Move(Position(7,6),Position(7,5)),
      Move(Position(2,0),Position(4,2)),
      Move(Position(7,7),Position(7,6)),
      Move(Position(7,0),Position(7,4)))
    chess.runMoves shouldBe expected
  }

}
