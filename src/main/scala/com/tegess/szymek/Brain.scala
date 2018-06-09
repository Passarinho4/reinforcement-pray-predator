package com.tegess.szymek

import com.tegess.Move

class Brain(boardSize: Int) {

  private val moves: Array[Array[Int]] = Array.ofDim(boardSize, 8)

  def initiateMoves() = {
    for{row <- moves.indices
      move <- moves(row).indices} {
      moves(row)(move) = 0
    }
  }

  def getMove(possibleMoves: Move) = {

  }



}
