package com.tegess

import java.util.Random

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

case class Board(n: Int) {
  private val table: Array[Array[Field]] = Array.ofDim[Field](n, n)
  private val rabbits: ArrayBuffer[Rabbit] = ArrayBuffer.empty
  private val wolfs: ArrayBuffer[Wolf] = ArrayBuffer.empty

  def isEmpty(position: Position) = table(position.row)(position.col).isEmpty()

  def moveCharacter(character: (Movable with Character), move: Move) = {
    table(character.position.row)(character.position.col).removeCharacter(character)
    val field = table(move.toPosition.row)(move.toPosition.col)
    field.addCharacter(character)
    character.doMove(move)
  }

  def checkConstraints() = {
    for {row <- table
         field <- row} {
      if (field.containsRabbit()) {
        field.removeGrass()
      }

      if (field.containsWolf()) {
        val removedRabbits = field.removeRabbits()
        rabbits --= removedRabbits
      }
    }
  }

  def initiate(): Unit = {
    def insertEmptyFields(): Unit = {
      for {row <- 0 until n
           column <- 0 until n} {
        table(row)(column) = Field(ArrayBuffer.empty, Position(row, column))
      }
    }

    @tailrec
    def findEmptyField(): Field = {
      val row = Board.random.nextInt(table.length)
      val col = Board.random.nextInt(table(row).length)
      if (table(row)(col).characters.isEmpty) {
        table(row)(col)
      } else {
        findEmptyField()
      }
    }

    def insertRandom(producer: Position => Character, n: Int): Unit = {
      for (_ <- 0 until n) {
        val field = findEmptyField()
        field.addCharacter(producer(field.position))
      }
    }

    insertEmptyFields()
    insertRandom((position: Position) => Grass(position), Board.grassInitCount)
    insertRandom((position: Position) => {
      val rabbit = Rabbit(0, position)
      rabbits += rabbit
      rabbit
    }, Board.rabbitInitCount)
    insertRandom((position: Position) => {
      val wolf = Wolf(0, position)
      wolfs += wolf
      wolf
    }, Board.wolfInitCount)
  }

  def printBoard(): Unit = {
    for (row <- table) {
      for (field <- row) {
        field.printField()
      }
      println()
    }
  }

  def doRound(): Unit = {
    for (rabbit <- rabbits) {
      val moves = rabbit.possibleMoves
      moves.head
      moveCharacter(rabbit, scala.util.Random.shuffle(moves).head)
    }
    for (wolf <- wolfs) {
      val moves = wolf.possibleMoves
      moves.head
      moveCharacter(wolf, scala.util.Random.shuffle(moves).head)
    }
    checkConstraints()
  }
}

object Board {
  val random: Random = new Random()
  val defaultBoardSize: Int = 20
  val grassInitCount: Int = 80
  val rabbitInitCount: Int = 10
  val wolfInitCount: Int = 5
}

case class Field(var characters: ArrayBuffer[Character], position: Position) {
  def containsRabbit() = characters.collect { case r: Rabbit => r }.nonEmpty

  def containsWolf() = characters.collect { case w: Wolf => w }.nonEmpty

  def removeGrass(): Seq[Grass] = {
    val removed = characters.collect { case e@(_: Grass) => e }.toList
    characters --= removed
    removed
  }

  def removeRabbits(): Seq[Rabbit] = {
    val removed = characters.collect { case e@(_: Rabbit) => e }.toList
    characters --= removed
    removed
  }

  def addCharacter(character: Character): Unit = characters += character

  def removeCharacter(character: Character): Unit = characters -= character

  def printField(): Unit = {
    if (characters.isEmpty) {
      print("[O]")
    } else if (characters.size == 1) {
      print(s"[${characters.head.letter}]")
    } else {
      print(s"[${characters.map(_.letter).mkString("")}]")
    }
  }

  def isEmpty() = characters.isEmpty
}

trait Character {
  val color: Color
  val letter: String
  var position: Position
}

trait Movable {
  this: Character =>
  def possibleMoves: Seq[Move] = {
    val moves = ArrayBuffer.empty[Move]
    //top
    if (position.row != 0) {
      moves += Move(Position(position.row - 1, position.col))
    }
    //top-left
    if (position.row != 0 && position.col != 0) {
      moves += Move(Position(position.row - 1, position.col - 1))
    }
    //top-right
    if (position.row != 0 && position.col != Board.defaultBoardSize - 1) {
      moves += Move(Position(position.row - 1, position.col + 1))
    }
    //left
    if (position.col != 0) {
      moves += Move(Position(position.row, position.col - 1))
    }
    //right
    if (position.col != Board.defaultBoardSize - 1) {
      moves += Move(Position(position.row, position.col + 1))
    }
    //down
    if (position.row != Board.defaultBoardSize - 1) {
      moves += Move(Position(position.row + 1, position.col))
    }
    //down-left
    if (position.row != Board.defaultBoardSize - 1 && position.col != 0) {
      moves += Move(Position(position.row + 1, position.col - 1))
    }
    //down-right
    if (position.row != Board.defaultBoardSize - 1 && position.col != Board.defaultBoardSize - 1) {
      moves += Move(Position(position.row + 1, position.col + 1))
    }
    moves.toSeq
  }

  def doMove(move: Move): Unit = {
    position = move.toPosition
  }
}

case class Move(toPosition: Position)

case class Grass(var position: Position) extends Character {
  override val color: Color = Color("green")
  override val letter: String = Utils.ANSI_GREEN + "G" + Utils.ANSI_RESET
}

case class Rabbit(age: Int, var position: Position) extends Character with Movable {
  override val color: Color = Color("blue")
  override val letter: String = Utils.ANSI_BLUE + "R" + Utils.ANSI_RESET

}

case class Wolf(age: Int, var position: Position) extends Character with Movable {
  override val color: Color = Color("red")
  override val letter: String = Utils.ANSI_RED + "W" + Utils.ANSI_RESET
}

case class Color(hex: String) extends AnyVal

case class Position(row: Int, col: Int)

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