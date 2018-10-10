package edu.jproyo

package object algebra {

  case class Position(column: Int, row: Int){
    override def toString: String = s"${(column+'a').toChar}${8-row}"
  }
  case class Move(from: Position, to: Position){
    override def toString: String = s"from:$from-to:$to"
  }

  sealed trait Player
  case object PlayerOne extends Player
  case object PlayerTwo extends Player

  sealed trait Piece {
    def player: Player

    override def toString: String =
      player match {
        case PlayerOne => getClass.getSimpleName
        case PlayerTwo => getClass.getSimpleName.head.toLower + getClass.getSimpleName.tail
      }
  }

  case class Pawn(player: Player) extends Piece
  case class Knight(player: Player) extends Piece
  case class Bishop(player: Player) extends Piece
  case class Rook(player: Player) extends Piece
  case class Queen(player: Player) extends Piece
  case class King(player: Player) extends Piece

  type Table = Map[Position, Option[Piece]]

  abstract class Board {

    var table: Table

    def get: Table = table

    def update: (Move) => Move = move => {
      table = table + (move.to -> table(move.from)) + (move.from -> None)
      move
    }

    def updateAndPrint: (Move) => Unit = update andThen printTable

    def printTable: (Move) => Unit = _ => println(this)


    override def toString: String =
      get.map {
        case (pos, piece) => s"Position: $pos - Piece: ${piece.fold("Empty")(_.toString)}}"
      }.mkString("\n")
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
      row match {
        case 0 => edgePiece(column, PlayerTwo)
        case 1 => Some(Pawn(PlayerTwo))
        case 6 => Some(Pawn(PlayerOne))
        case 7 => edgePiece(column, PlayerOne)
        case _ => None
      }

    private def edgePiece(column: Int, player: Player): Option[Piece] = {
      column match {
        case 0 => Some(Rook(player))
        case 1 => Some(Knight(player))
        case 2 => Some(Bishop(player))
        case 3 => Some(King(player))
        case 4 => Some(Queen(player))
        case 5 => Some(Bishop(player))
        case 6 => Some(Knight(player))
        case 7 => Some(Rook(player))
      }
    }

  }

}
