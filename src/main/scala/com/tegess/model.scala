package com.tegess

import java.util.Random

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

case class Board(n: Int) {
  private val table: Array[Array[Field]] = Array.ofDim[Field](n,n)

  def initiate(): Unit = {
    def insertEmptyFields(): Unit = {
      for {row <- 0 until n
           column <- 0 until n} {
        table(row)(column) = Field(ArrayBuffer.empty)
      }
    }

    @tailrec
    def findEmptyField(): Field = {
      val row = Board.random.nextInt(table.length)
      val col = Board.random.nextInt(table(row).length)
      if(table(row)(col).characters.isEmpty) {
        table(row)(col)
      } else {
        findEmptyField()
      }
    }
    def insertRandom(producer: () => Character, n: Int): Unit = {
      for (_ <- 0 until n) {
        findEmptyField().addCharacter(producer())
      }
    }

    insertEmptyFields()
    insertRandom(() => Grass(), Board.grassInitCount)
    insertRandom(() => Rabbit(0), Board.rabbitInitCount)
    insertRandom(() => Wolf(0), Board.wolfInitCount)
  }

  def printBoard(): Unit = {
    for (row <- table){
      for(field <- row) {
        field.printField()
      }
      println()
    }
  }

}

object Board {
  val random: Random = new Random()
  val defaultBoardSize: Int = 10
  val grassInitCount: Int = 15
  val rabbitInitCount: Int = 10
  val wolfInitCount: Int = 5
}

case class Field(characters: ArrayBuffer[Character]) {
  def addCharacter(character: Character): Unit = characters += character
  def printField(): Unit = {
    if(characters.isEmpty) {
      print("[O]")
    } else if(characters.size == 1) {
      print(s"[${characters.head.letter}]")
    } else {
      print(s"[${characters.map(_.letter).mkString("")}]")
    }
  }
}

trait Character {
  val color: Color
  val letter: String
}

case class Grass() extends Character {
  override val color: Color = Color("green")
  override val letter: String = Utils.ANSI_GREEN + "G" + Utils.ANSI_RESET
}

case class Rabbit(age: Int) extends Character {
  override val color: Color = Color("blue")
  override val letter: String = Utils.ANSI_BLUE + "R" + Utils.ANSI_RESET
}

case class Wolf(age: Int) extends Character {
  override val color: Color = Color("red")
  override val letter: String = Utils.ANSI_RED + "W" + Utils.ANSI_RESET
}

case class Color(hex: String) extends AnyVal

object Utils {
  val ANSI_RESET = "\u001B[0m"
  val ANSI_BLACK = "\u001B[30m"
  val ANSI_RED = "\u001B[31m"
  val ANSI_GREEN = "\u001B[32m"
  val ANSI_YELLOW = "\u001B[33m"
  val ANSI_BLUE = "\u001B[34m"
  val ANSI_PURPLE = "\u001B[35m"
  val ANSI_CYAN = "\u001B[36m"
  val ANSI_WHITE = "\u001B[37m"
}