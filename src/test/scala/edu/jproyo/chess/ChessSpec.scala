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
    val board = Board()
    val expected = Map(
      Position(0,0) -> Some(Rook(PlayerTwo)),
      Position(0,1) -> Some(Pawn(PlayerTwo)),
      Position(0,2) -> None,
      Position(0,3) -> None,  
      Position(0,4) -> None,  
      Position(0,5) -> None,  
      Position(0,6) -> Some(Pawn(PlayerOne)),
      Position(0,7) -> Some(Rook(PlayerOne)),
      Position(1,0) -> Some(Knight(PlayerTwo)),
      Position(1,1) -> Some(Pawn(PlayerTwo)),
      Position(1,2) -> None,  
      Position(1,3) -> None,  
      Position(1,4) -> None,  
      Position(1,5) -> None,  
      Position(1,6) -> Some(Pawn(PlayerOne)),
      Position(1,7) -> Some(Knight(PlayerOne)),
      Position(2,0) -> Some(Bishop(PlayerTwo)),
      Position(2,1) -> Some(Pawn(PlayerTwo)),
      Position(2,2) -> None,  
      Position(2,3) -> None,  
      Position(2,4) -> None,  
      Position(2,5) -> None,  
      Position(2,6) -> Some(Pawn(PlayerOne)),
      Position(2,7) -> Some(Bishop(PlayerOne)),
      Position(3,0) -> Some(King(PlayerTwo)),
      Position(3,1) -> Some(Pawn(PlayerTwo)),
      Position(3,2) -> None,  
      Position(3,3) -> None,  
      Position(3,4) -> None,  
      Position(3,5) -> None,  
      Position(3,6) -> Some(Pawn(PlayerOne)),
      Position(3,7) -> Some(King(PlayerOne)),
      Position(4,0) -> Some(Queen(PlayerTwo)),
      Position(4,1) -> Some(Pawn(PlayerTwo)),
      Position(4,2) -> None,  
      Position(4,3) -> None,  
      Position(4,4) -> None,  
      Position(4,5) -> None,  
      Position(4,6) -> Some(Pawn(PlayerOne)),
      Position(4,7) -> Some(Queen(PlayerOne)),
      Position(5,0) -> Some(Bishop(PlayerTwo)),
      Position(5,1) -> Some(Pawn(PlayerTwo)),
      Position(5,2) -> None,  
      Position(5,3) -> None,  
      Position(5,4) -> None,  
      Position(5,5) -> None,  
      Position(5,6) -> Some(Pawn(PlayerOne)),
      Position(5,7) -> Some(Bishop(PlayerOne)),
      Position(6,0) -> Some(Knight(PlayerTwo)),
      Position(6,1) -> Some(Pawn(PlayerTwo)),
      Position(6,2) -> None,  
      Position(6,3) -> None,  
      Position(6,4) -> None,  
      Position(6,5) -> None,  
      Position(6,6) -> Some(Pawn(PlayerOne)),
      Position(6,7) -> Some(Knight(PlayerOne)),
      Position(7,0) -> Some(Rook(PlayerTwo)),
      Position(7,1) -> Some(Pawn(PlayerTwo)),
      Position(7,2) -> None,  
      Position(7,3) -> None,  
      Position(7,4) -> None,  
      Position(7,5) -> None,  
      Position(7,6) -> Some(Pawn(PlayerOne)),
      Position(7,7) -> Some(Rook(PlayerOne)))
    board.get shouldBe expected
  }


  "Step 3" should "udpate board ignoring valids" in {
    val board = Board()
    board.update(Move(Position(0,1), Position(0,3)))
    board.get(Position(0,1)) shouldBe None
    board.get(Position(0,3)) shouldBe Some(Pawn(PlayerTwo))
  }


  "Step 4" should "udpate board and print status" in {
    val board = Board()
    board.updateAndPrint(Move(Position(0,1), Position(0,3)))
    board.get(Position(0,1)) shouldBe None
    board.get(Position(0,3)) shouldBe Some(Pawn(PlayerTwo))
  }


  "Step 5" should "udpate board with invalid piece movement" in {
    val board = Board()
    val result = board.update(Move(Position(2,7), Position(2,5)))
    result shouldBe Left(WrongMovementOfPiece)
  }

}
