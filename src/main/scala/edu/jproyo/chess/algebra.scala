package edu.jproyo

package object algebra {

  case class Position(column: Int, row: Int){
    override def toString: String = s"${(column+'a').toChar}${8-row}"
  }
  case class Move(from: Position, to: Position){
    override def toString: String = s"from:$from-to:$to"
  }

  sealed trait Piece
  case object Pawn extends Piece
  case object Knight extends Piece
  case object Bishop extends Piece
  case object Rook extends Piece
  case object Queen extends Piece
  case object King extends Piece

  type Table = Map[Position, Option[Piece]]

  abstract class Board {

    var table: Table

    def get: Table = table

    def update(move: Move): Unit =
      table = table + (move.to  -> table(move.from)) + (move.from -> None)
  }

  object Board{

    def apply(): Board =
      new Board() {
        var table: Table = initialize
      }

    private def initialize: Table =
      (for {
        column <- 0 to 7
        row    <- 0 to 7
      } yield (Position(column, row), calculatePiece(column, row))).toMap


    private def calculatePiece(column: Int, row: Int): Option[Piece] =
      if(row == 6 || row == 1) Some(Pawn)
      else if(row == 0 || row == 7) edgePiece(column, row)
      else None

    private def edgePiece(column: Int, row: Int): Option[Piece] = {
      if(column == row) Some(Rook)
      else column match {
        case 0 => Some(Rook)
        case 1 => Some(Knight)
        case 2 => Some(Bishop)
        case 3 => Some(King)
        case 4 => Some(Queen)
        case 5 => Some(Bishop)
        case 6 => Some(Knight)
        case 7 => Some(Rook)
      }
    }

  }

}
