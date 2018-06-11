package com.tegess.szymek

import java.util.Random

import com.tegess.{Direction, Move, Position}

class Brain(boardSize: Int) {

  private val moves: Array[Array[Int]] = Array.ofDim(boardSize*boardSize, 8)

  private val random = new Random()
  def initiateMoves() = {
    for{row <- moves.indices
      move <- moves(row).indices} {
      moves(row)(move) = random.nextInt(3)
    }
  }

  def getMove(possibleMoves: Seq[Move], from: Position, risk: Int): Move = {
    val r = random.nextInt(100)
    val field = from.row * boardSize + from.col
    val possible = possibleMoves.map(_.direction.i)
    val mostProfitableDirectionId = if(r < risk) {
      scala.util.Random.shuffle(moves(field).zipWithIndex
        .filter{case (_, index) => possible.contains(index)}
        .toList)
        .head._2
    } else {
      moves(field).zipWithIndex
        .filter{case (_, index) => possible.contains(index)}
        .maxBy(_._1)
        ._2
    }
    possibleMoves.filter(_.direction.i == mostProfitableDirectionId).head
  }

  def update(position: Position, direction: Direction, newValue: Int) = {
    val field = position.row * boardSize + position.col
    moves(field)(direction.i) = newValue
  }

  def printBrain() = {
    for(row <- moves) {
      for (e <- row) {
        print(s"$e,")
      }
      println()
    }
  }
}
