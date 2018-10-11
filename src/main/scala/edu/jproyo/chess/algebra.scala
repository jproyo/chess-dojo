package edu.jproyo

package object algebra {

  case class Position(column: Int, row: Int){

    def isInside: Boolean = (0 to 7).contains(column) && (0 to 7).contains(row)

    def isDiag(another: Position): Boolean = {
      val fromP = Math.abs(row - column)
      val toP = Math.abs(another.row - another.column)
      return fromP == toP
    }

    override def toString: String = s"${(column+'a').toChar}${8-row}"
  }

  case class Move(from: Position, to: Position){

    def insideBoard: Boolean = from.isInside && to.isInside

    def isDiag: Boolean = from.isDiag(to)

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

  type Table = Map[Position, Piece]

  sealed trait InvalidMove
  case object OutofBoardMove extends InvalidMove
  case object WrongMovementOfPiece extends InvalidMove
  case object NoPieceFoundInPosition extends InvalidMove
  case object PieceBlocked extends InvalidMove

  trait MoveValidator{

    def validOutOfBoard(table: Table, move: Move): Either[InvalidMove, Move]

    def validWrongMovement(table: Table, move: Move): Either[InvalidMove, Move]

    def validPieceBlocked(table: Table, move: Move): Either[InvalidMove, Move]

    def valid(table: Table, move: Move): Either[InvalidMove, Move] =
      for{
        mOut <- validOutOfBoard(table, move)
        mWrong <- validWrongMovement(table, mOut)
        mBlocked <- validPieceBlocked(table, mWrong)
      } yield mBlocked

  }

  implicit val chessValidator = new MoveValidator {
    override def validOutOfBoard(table: Table, move: Move): Either[InvalidMove, Move] =
      if(move.insideBoard) Right(move) else Left(OutofBoardMove)


    def validMove(table: Table, move: Move, piece: Piece): Either[InvalidMove, Move] = piece match {
      case p@Pawn(_) => p.validMove(table, move)
      case p@Knight(_) => p.validMove(table, move)
      case p@Queen(_) => p.validMove(table, move)
      case p@King(_) => p.validMove(table, move)
      case p@Rook(_) => p.validMove(table, move)
      case p@Bishop(_) => p.validMove(table, move)
    }

    override def validWrongMovement(table: Table, move: Move): Either[InvalidMove, Move] =
      table.get(move.from).fold(Left(NoPieceFoundInPosition))(piece => return validMove(table, move, piece))

    override def validPieceBlocked(table: Table, move: Move): Either[InvalidMove, Move] = Right(move)
  }

  trait MovePieceRule{
    def validMove(table: Table, move: Move): Either[InvalidMove, Move]
  }

  implicit class PawnRule(pawn: Pawn) extends MovePieceRule{
    override def validMove(table: Table, move: Move): Either[InvalidMove, Move] = {
      if(move.from.column != move.to.column) return Left(WrongMovementOfPiece)
      val allowed = pawn.player match {
        case PlayerOne => if (move.from.row == 6) 2 else 1
        case PlayerTwo => if (move.from.row == 1) 2 else 1
      }
      if(Math.abs(move.from.row - move.to.row) <= allowed){
        val fromSq = Math.min(move.from.row, move.to.row) + 1
        val toSq = Math.max(move.from.row, move.to.row)
        val result = for {
          i     <- fromSq to toSq
          piece <- table.get(Position(move.from.column, i))
        } yield piece
        if(result.nonEmpty) return Left(WrongMovementOfPiece)
        else return Right(move)
      } else { Left(WrongMovementOfPiece) }
    }
  }

  implicit class RookRule(rook: Rook) extends MovePieceRule {
    override def validMove(table: Table, move: Move): Either[InvalidMove, Move] =
      if(!move.isDiag) return Right(move) else Left(WrongMovementOfPiece)
  }

  implicit class KnightRule(knight: Knight) extends MovePieceRule {
    override def validMove(table: Table, move: Move): Either[InvalidMove, Move] = {
      val cond = move.from.column != move.to.column &&
                 move.from.row != move.to.row &&
                 !move.isDiag
      if(cond) Right(move) else Left(WrongMovementOfPiece)
    }
  }

  implicit class KingRule(king: King) extends MovePieceRule {
    override def validMove(table: Table, move: Move): Either[InvalidMove, Move] = {
      val row = Math.abs(move.from.row - move.to.row)
      val col = Math.abs(move.from.column - move.to.column)
      if(row <= 1 && col <= 1) Right(move)
      else Left(WrongMovementOfPiece)
    }
  }

  implicit class QueenRule(queen: Queen) extends MovePieceRule {
    override def validMove(table: Table, move: Move): Either[InvalidMove, Move] = Right(move)
  }

  implicit class BishopRule(bishop: Bishop) extends MovePieceRule {
    override def validMove(table: Table, move: Move): Either[InvalidMove, Move] = {
      if(move.isDiag) Right(move) else Left(WrongMovementOfPiece)
    }
  }


  sealed trait Result
  case object Continue extends Result
  case object Check extends Result
  case object CheckMate extends Result


  abstract class Board {

    var table: Table

    def get: Table = table

    type PlayState = Either[InvalidMove, Result]

    def update(move: Move)(implicit validator: MoveValidator): Either[InvalidMove, Move] = {
      validator.valid(table, move).fold(Left(_), m => {
        table = table + (m.to -> table(m.from))  - m.from
        Right(m)
      })
    }

    def updateAndPrint(move: Move)(implicit validator: MoveValidator): Unit =
      printTable(update(move))


    def printTable: Either[InvalidMove, Move] => Unit = _.fold(println, println)


    override def toString: String =
      get.map {
        case (pos, piece) => s"Position: $pos - Piece: $piece"
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
        if !(2 to 5).contains(row)
      } yield (Position(column, row), calculatePiece(column, row))).toMap


    private def calculatePiece(column: Int, row: Int): Piece =
      row match {
        case 0 => edgePiece(column, PlayerTwo)
        case 1 => Pawn(PlayerTwo)
        case 6 => Pawn(PlayerOne)
        case 7 => edgePiece(column, PlayerOne)
      }

    private def edgePiece(column: Int, player: Player): Piece = {
      column match {
        case 0 => Rook(player)
        case 1 => Knight(player)
        case 2 => Bishop(player)
        case 3 => King(player)
        case 4 => Queen(player)
        case 5 => Bishop(player)
        case 6 => Knight(player)
        case 7 => Rook(player)
      }
    }

  }

}
