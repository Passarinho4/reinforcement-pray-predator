package com.tegess

import java.util.Random

import com.tegess.szymek.Brain

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

case class Board(n: Int, brain: Brain) {
  private val table: Array[Array[Field]] = Array.ofDim[Field](n, n)

  private val grasses: ArrayBuffer[Grass] = ArrayBuffer.empty
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
    def grassConstraint(field: Field) = if (field.containsRabbit()) {
      val removedGrass = field.removeGrass()
      if(removedGrass.nonEmpty) {
        field.characters
          .collect {case r:Rabbit => r}
          .foreach { r: Rabbit => r.hungry = 1 }
      }
      grasses --= removedGrass
    }
    def rabbitConstraint(field: Field) = if (field.containsWolf()) {
      val removedRabbits = field.removeRabbits()
      rabbits --= removedRabbits
    }
    def rabbitBirthConstraint(field: Field) = {
      for (i <- 0 until field.rabbitPairsCount()) {
        val f = findEmptyField()
        val rabbitChild = Rabbit(0, f.position, c = Utils.ANSI_CYAN)
        f.addCharacter(rabbitChild)
        rabbits += rabbitChild
      }
    }
    def wolfBirthConstraint(field: Field) = {
      for (i <- 0 until field.wolfPairsCount()) {
        val f = findEmptyField()
        val wolfChild = Wolf(0, f.position, Utils.ANSI_PURPLE)
        f.addCharacter(wolfChild)
        wolfs += wolfChild
      }
    }

    def rabbitDeathConstraint(field: Field) = {
      val removedRabbits = field.characters.collect{ case r: Rabbit => r}.filter(_.hungry > 100)
      field.characters --= removedRabbits
      rabbits --= removedRabbits

    }
    for {row <- table
         field <- row} {
      grassConstraint(field)
      rabbitConstraint(field)
      rabbitBirthConstraint(field)
      wolfBirthConstraint(field)
      rabbitDeathConstraint(field)
    }
  }

  @tailrec
  private def findEmptyField(): Field = {
    val row = Board.random.nextInt(table.length)
    val col = Board.random.nextInt(table(row).length)
    if (table(row)(col).characters.isEmpty) {
      table(row)(col)
    } else {
      findEmptyField()
    }
  }

  def initiate(): Unit = {
    def insertEmptyFields(): Unit = {
      for {row <- 0 until n
           column <- 0 until n} {
        table(row)(column) = Field(ArrayBuffer.empty, Position(row, column))
        if(row < 7 & column < 7) {
          table(row)(column).characters += Grass(Position(row, column))
          grasses += Grass(Position(row, column))
        }
        if(row > 13 & column >13) {
          val w = Wolf(0, Position(row, column))
          table(row)(column).characters += w
          wolfs += w
        }
      }
    }

    def insertRandom(producer: Position => Character, n: Int): Unit = {
      for (_ <- 0 until n) {
        val field = findEmptyField()
        field.addCharacter(producer(field.position))
      }
    }

    insertEmptyFields()
    insertRandom((position: Position) => {
      val grass = Grass(position)
      grasses += grass
      grass
    }, Board.grassInitCount)
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
      val profitableMove = brain.getMove(moves, rabbit.position, rabbit.hungry)
      val beforePosition = rabbit.position
      moveCharacter(rabbit, profitableMove)
      brain.update(beforePosition, profitableMove.direction, countProfitability(profitableMove))
    }
    for (wolf <- wolfs) {
      val moves = wolf.possibleMoves
      //moveCharacter(wolf, scala.util.Random.shuffle(moves).head)
    }
    checkConstraints()
  }

  def countDistance(position1: Position, position2: Position): Double = {
    Math.sqrt(Math.pow(position2.row - position1.row, 2) + Math.pow(position2.col - position1.col, 2))
  }

  def countProfitability(move: Move): Int = {
    def countProfits(position: Position): Int = {
      val distances = for(grass <- grasses) yield { countDistance(position, grass.position) }
      distances
        .take(3)
        .map(Board.maxDistance - _)
        .sum
        .toInt
    }

    def countLosses(position: Position): Int = {
      val distances = for(wolf <- wolfs) yield {countDistance(position, wolf.position)}
      distances
        .sorted
        .take(3)
        .map(Board.maxDistance - _)
        .sum
        .toInt
    }

    val destination = move.toPosition

    val profits = countProfits(destination)
    val losses = countLosses(destination)
    profits - losses
  }


}

object Board {
  val maxDistance = Math.sqrt(2) * defaultBoardSize
  val random: Random = new Random()
  val defaultBoardSize: Int = 20
  val grassInitCount: Int = 0
  val rabbitInitCount: Int = 10
  val wolfInitCount: Int = 0
}

case class Field(var characters: ArrayBuffer[Character], position: Position) {

  //def rabbitPairsCount() = characters.collect { case r:Rabbit => r}.size / 2
  def rabbitPairsCount() = 0

  //def wolfPairsCount() = characters.collect { case r:Wolf => r}.size / 2
  def wolfPairsCount() = 0

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
      moves += Move(Position(position.row - 1, position.col), position, Top)
    }
    //top-left
    if (position.row != 0 && position.col != 0) {
      moves += Move(Position(position.row - 1, position.col - 1), position, TopLeft)
    }
    //top-right
    if (position.row != 0 && position.col != Board.defaultBoardSize - 1) {
      moves += Move(Position(position.row - 1, position.col + 1), position, TopRight)
    }
    //left
    if (position.col != 0) {
      moves += Move(Position(position.row, position.col - 1), position, Left)
    }
    //right
    if (position.col != Board.defaultBoardSize - 1) {
      moves += Move(Position(position.row, position.col + 1), position, Right)
    }
    //down
    if (position.row != Board.defaultBoardSize - 1) {
      moves += Move(Position(position.row + 1, position.col), position, Down)
    }
    //down-left
    if (position.row != Board.defaultBoardSize - 1 && position.col != 0) {
      moves += Move(Position(position.row + 1, position.col - 1), position, DownLeft)
    }
    //down-right
    if (position.row != Board.defaultBoardSize - 1 && position.col != Board.defaultBoardSize - 1) {
      moves += Move(Position(position.row + 1, position.col + 1), position, DownRight)
    }
    moves.toSeq
  }

  def doMove(move: Move): Unit = {
    position = move.toPosition
  }
}

trait Direction {
  val i: Int
}
case object TopLeft extends Direction {
  override val i: Int = 0
}
case object Top extends Direction {
  override val i: Int = 1
}
case object TopRight extends Direction {
  override val i: Int = 2
}
case object Right extends Direction {
  override val i: Int = 3
}
case object DownRight extends Direction {
  override val i: Int = 4
}
case object Down extends Direction {
  override val i: Int = 5
}
case object DownLeft extends Direction {
  override val i: Int = 6
}
case object Left extends Direction {
  override val i: Int = 7
}

case class Move(toPosition: Position, fromPosition: Position, direction: Direction)

case class Grass(var position: Position) extends Character {
  override val color: Color = Color("green")
  override val letter: String = Utils.ANSI_GREEN + "G" + Utils.ANSI_RESET
}

case class Rabbit(age: Int,
                  var position: Position,
                  var hungry: Int = 1,
                  c: String = Utils.ANSI_BLUE) extends Character with Movable {
  override val color: Color = Color("blue")
  override val letter: String = c + "R" + Utils.ANSI_RESET

  override def doMove(move: Move): Unit = {
    super.doMove(move)
    this.hungry = this.hungry + 1
  }
}

case class Wolf(age: Int, var position: Position, c: String = Utils.ANSI_RED) extends Character with Movable {
  override val color: Color = Color("red")
  override val letter: String = c + "W" + Utils.ANSI_RESET
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