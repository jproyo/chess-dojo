package edu.jproyo.chess

import com.whitehatgaming.UserInputFile

package object program {

  case class Position(column: Int, row: Int){
    override def toString: String = s"${(column+'a').toChar}${8-row}"
  }
  case class Move(from: Position, to: Position){
    override def toString: String = s"from:$from-to:$to"
  }

  class Chess(fileName: String) {

    lazy val repository = new UserInputFile(fileName)

    def next: Option[Move] = repository.nextMove match
    {
      case Array(a,b,c,d) => Some(Move(Position(a, b), Position(c, d)))
      case null => None
    }

    def runMoves: List[Move] =
      next.fold(List.empty[Move])(_ :: runMoves)

  }

}
