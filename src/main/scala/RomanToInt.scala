package org.example.copper


import scala.util.Try

/*
Symbol       Value
I             1
IV            4
V             5
IX            9
X             10
XL            40
L             50
XC            90
C             100
CD            400
D             500
CM            900
M             1000
*/

object RomanToInt {

  val romanToIntMap: Map[String, Int] = Map(
    "M" -> 1000,
    "CM" -> 900,
    "D" -> 500,
    "CD" -> 400,
    "C" -> 100,
    "XC" -> 90,
    "L" -> 50,
    "XL" -> 40,
    "X" -> 10,
    "IX" -> 9,
    "V" -> 5,
    "IV" -> 4,
    "I" -> 1
  )

  def romanToInt(input: String): Either[String, Int] = {
    val doubleLetterKeys: Seq[String] = romanToIntMap.keys.toSeq.filter(_.length == 2)

    var result: Int = 0
    var phrase: String = input
    var previousValue: Int = 0
    var theSameLetterCounter: Int = 0
    var previousLetter: String = ""

    def singlesValidation(oneLetterPrefix: String): Either[String, Int] = {
      val value = romanToIntMap.get(oneLetterPrefix).fold(return Left(s"Illegal input $oneLetterPrefix"))(ident => ident)
      if (value > previousValue && previousValue != 0) return Left(s"Illegal input right number is higher than left $input | $oneLetterPrefix")
      if (previousLetter != oneLetterPrefix) {
        previousLetter = oneLetterPrefix
        theSameLetterCounter = 1
        Right(value)
      } else if (theSameLetterCounter >= 3) {
        Left(s"Illegal input $input -> 4 times $oneLetterPrefix")
      } else {
        theSameLetterCounter = theSameLetterCounter + 1
        Right(value)
      }
    }

    def iterate(step: Int, value: Int): Either[String, Int] = {
      result += value
      previousValue = value
      if (phrase.length == step) Right(result)
      else Left("Next iteration available")
    }

    while (phrase.nonEmpty) {
      val oneLetterPrefix: String = phrase.head.toString
      Try(phrase.substring(0, 2))
        .toOption
        .flatMap(key => doubleLetterKeys.find(_ == key))
        .fold {
          singlesValidation(oneLetterPrefix) match {
            case Right(value) => iterate(1, value) match {
              case Right(value) => return Right(value)
              case Left(_) => phrase = phrase.substring(1)
            }
            case Left(error) => return Left(error)
          }
        } { prefix =>
          val value = romanToIntMap.get(prefix).fold(return Left(s"Illegal input $prefix"))(ident => ident)
          iterate(2, value) match {
            case Right(value) => return Right(value)
            case Left(_) => phrase = phrase.substring(2)
          }
        }
    }
    Right(result)
  }

  def main(args: Array[String]): Unit = {
    if (args.nonEmpty) args.map(romanToInt).foreach {
      case Right(value) => println(s"Successfully parsed argument: $value")
      case Left(value) => println(s"Error while parsing argument: $value")
    } else {
      println(s"This is default run without arguments. Parsing MMDCCXXXIX. Result ${romanToInt("MMDCCXXXIX")}")
      println(s"This is default run without arguments. Parsing CCCC. Result ${romanToInt("CCCCC")}")
      println(s"This is default run without arguments. Parsing MCMXCIV. Result ${romanToInt("MCMXCIV")}")
      println(s"This is default run without arguments. Parsing CMDM. Result ${romanToInt("CMDM")}")
      println(s"This is default run without arguments. Parsing MCD. Result ${romanToInt("MCD")}")
    }
  }

}
