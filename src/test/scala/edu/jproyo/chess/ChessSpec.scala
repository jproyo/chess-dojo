package edu.jproyo
package chess

import org.scalatest._
import program._
import algebra._

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

  "Step 2" should "model board with positions and pieces and initialize" in {
    val board = new Board
    val expected = Map(
      Position(0,0) -> Some(Rook),
      Position(0,1) -> Some(Pawn),
      Position(0,2) -> None,
      Position(0,3) -> None,  
      Position(0,4) -> None,  
      Position(0,5) -> None,  
      Position(0,6) -> Some(Pawn),
      Position(0,7) -> Some(Rook),
      Position(1,0) -> Some(Knight),
      Position(1,1) -> Some(Pawn),
      Position(1,2) -> None,  
      Position(1,3) -> None,  
      Position(1,4) -> None,  
      Position(1,5) -> None,  
      Position(1,6) -> Some(Pawn),
      Position(1,7) -> Some(Knight),
      Position(2,0) -> Some(Bishop),
      Position(2,1) -> Some(Pawn),
      Position(2,2) -> None,  
      Position(2,3) -> None,  
      Position(2,4) -> None,  
      Position(2,5) -> None,  
      Position(2,6) -> Some(Pawn),
      Position(2,7) -> Some(Bishop),
      Position(3,0) -> Some(King),
      Position(3,1) -> Some(Pawn),
      Position(3,2) -> None,  
      Position(3,3) -> None,  
      Position(3,4) -> None,  
      Position(3,5) -> None,  
      Position(3,6) -> Some(Pawn),
      Position(3,7) -> Some(King),
      Position(4,0) -> Some(Queen),
      Position(4,1) -> Some(Pawn),
      Position(4,2) -> None,  
      Position(4,3) -> None,  
      Position(4,4) -> None,  
      Position(4,5) -> None,  
      Position(4,6) -> Some(Pawn),
      Position(4,7) -> Some(Queen),
      Position(5,0) -> Some(Bishop),
      Position(5,1) -> Some(Pawn),
      Position(5,2) -> None,  
      Position(5,3) -> None,  
      Position(5,4) -> None,  
      Position(5,5) -> None,  
      Position(5,6) -> Some(Pawn),
      Position(5,7) -> Some(Bishop),
      Position(6,0) -> Some(Knight),
      Position(6,1) -> Some(Pawn),
      Position(6,2) -> None,  
      Position(6,3) -> None,  
      Position(6,4) -> None,  
      Position(6,5) -> None,  
      Position(6,6) -> Some(Pawn),
      Position(6,7) -> Some(Knight),
      Position(7,0) -> Some(Rook),
      Position(7,1) -> Some(Pawn),
      Position(7,2) -> None,  
      Position(7,3) -> None,  
      Position(7,4) -> None,  
      Position(7,5) -> None,  
      Position(7,6) -> Some(Pawn),
      Position(7,7) -> Some(Rook))
    board.get shouldBe expected
  }

}
