package edu.jproyo
package chess

import com.whitehatgaming.UserInputFile
import algebra._

package object program {

  class Chess(fileName: String) {

    lazy val repository = new UserInputFile(fileName)

    def next: Option[Move] = repository.nextMove match
    {
      case Array(a,b,c,d) => Some(Move(Position(a, b), Position(c, d)))
      case null => None
    }

    def runMoves: List[Move] =
      next.fold(List.empty[Move])(_ :: runMoves)


    def play: Unit = {
      val board = Board()
      val result = runMoves.foldLeft(Right(Move(Position(0,0), Position(0,0))).asInstanceOf[Either[InvalidMove, Move]])((_, m) => board.update(m))
      println(result)
    }

  }

}
