package com.tegess

object Main extends App {

  clean()
  val board =  new Board(Board.defaultBoardSize)
  board.initiate()
  for (i <- 0 until 10000) {
    board.printBoard()
    board.doRound()
    Thread.sleep(200)
    clean()
  }


  def clean() = {
    System.out.print("\033[H\033[2J");
    System.out.flush();
  }
}
