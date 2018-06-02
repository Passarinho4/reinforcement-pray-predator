package com.tegess

object Main extends App {

  clean()
  for (i <- 0 until 10) {
    val board =  new Board(10)
    board.initiate()
    board.printBoard()
    Thread.sleep(1000)
    clean()
  }


  def clean() = {
    System.out.print("\033[H\033[2J");
    System.out.flush();
  }
}
