package com.tegess

import com.tegess.szymek.Brain

object Main extends App {

  clean()
  val brain = new Brain(Board.defaultBoardSize)
  brain.initiateMoves()
  val board =  new Board(Board.defaultBoardSize, brain)
  board.initiate()
  for (i <- 0 until 300) {
    board.printBoard()
    board.doRound()
    Thread.sleep(100)
    clean()
  }
  brain.printBrain()


  def clean() = {
    System.out.print("\033[H\033[2J");
    System.out.flush();
  }
}
