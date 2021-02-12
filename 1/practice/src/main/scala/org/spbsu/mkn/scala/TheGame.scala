package org.spbsu.mkn.scala

import scala.io.StdIn.readLine
import scala.util.Random

object TheGame {

  sealed trait GuessResult
  case class Correct(numTries: Int) extends GuessResult
  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException
  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException

  def generateNumberString(length: Int): String = {
    val gameAlphabet = ('a' to 'z').toList ++ ('A' to 'Z').toList ++ (0 to 9 ).toList
    val answer = Random.shuffle(gameAlphabet).take(length)
    answer.mkString("")
  }

  def checkFormat(userInput : String, rightLength : Int): Unit = {
    if (userInput.length != rightLength)
      throw new WrongNumberLengthException(rightLength, userInput.length)
    if (userInput.toSet.size != userInput.length)
      throw new RepeatingDigitsException
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    checkFormat(userInput, secret.length)

    if (userInput == secret)
      return Correct(numTries)

    val commonSymbols = secret.toSet.intersect(userInput.toSet)
    val cows = commonSymbols.count(it => userInput.indexOf(it) != secret.indexOf(it))
    val bulls = commonSymbols.size - cows
    Incorrect(bulls, cows)
  }

  def parseInput() : Int = {
    print("Введите длину загаданного слова: ")
    val inputData = readLine()
    var length = 0
    length = inputData.toInt
    length
  }

  def makeOneTry(secret : String, numTries : Int) : GuessResult = {
    println("Введите свою догадку: ")
    val usersTry = readLine()
    var result : GuessResult = null
    try {
      result = validate(secret, usersTry, numTries)
    } catch {
      case e : RepeatingDigitsException => println("В вашей попытке были повторяющиеся символы.")
      case e : WrongNumberLengthException => println(s"Попытка должна быть длины ${secret.length}")
    }
    result
  }

  def main(args: Array[String]): Unit = {
    val length = parseInput()
    val secret = generateNumberString(length)

    var numTries = 0
    var isTimeToFinish = false
    while (!isTimeToFinish) {
      val res = makeOneTry(secret, numTries)
      numTries += 1

      res match {
        case res : Correct =>
          isTimeToFinish = true
          println(s"Ура, вы угадали $secret с ${res.numTries} попытки")
        case res : Incorrect => println(s"Пока неверно. Вы угадали ${res.cows} коров и ${res.bulls} быков.")
        case _ =>
      }
    }
  }
}

