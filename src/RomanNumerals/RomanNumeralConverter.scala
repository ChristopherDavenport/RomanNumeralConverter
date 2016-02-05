package RomanNumerals

/**
  * Created by davenpcm on 2/5/2016.
  * Taken from and modified by several sources online.
  */
class RomanNumeralConverter {

  /**
    * Public method taking an Int and Returns a the string of Roman Numerals
    */
   def convertRoman(number: Int): String = {
     if (number <= 0) "Invalid number"
     else toRomanNumerals( number, RomanNumerals)
  }

  /**
    * Public Method taking a string of Roman Numeral and Converts to an Int
    */
  def convertRoman(numeral: String): Int = {
       fromRomanNumerals( numeral.toUpperCase(), RomanNumerals)
  }

  /**
    * This function takes our list of tuples and iterates through each.
    * The recursive step looks at the head tuple
    * and starts a string of the RomanNumeral as many times as it is divisible.
    * The remainder is passed through to the next tuple.
    */
  private def toRomanNumerals( number: Int, digits: List[(String, Int)]): String = digits match {
    case Nil => ""
    case x :: xs => x._1 * ( number / x._2) + toRomanNumerals(number % x._2, xs)
  }

  /**
    * This function maps letters to number values and recursively adds through the list in the appropriate format.
    * This works from highest to smallest through roman numerals.
    * If it matches then it increases the value and returns the entire list in case it is repeated
    * If it does not match it returns the whole string and moves to the next numeral
    */
  private def fromRomanNumerals(numeral: String, digits: List[(String, Int)]): Int = numeral match {
    case n if digits == Nil => 0
    case n if n.startsWith(digits.head._1) => digits.head._2 + fromRomanNumerals(numeral.substring(digits.head._1.length), digits)
    case _ => fromRomanNumerals(numeral, digits.tail)
  }

  /**
    * Just a simple List of Roman Numerals paired to their value.
    */
  private val RomanNumerals = List(
    ("M", 1000),
    ("CM", 900),
    ("D", 500),
    ("CD", 400),
    ("C", 100),
    ("XC", 90),
    ("L", 50),
    ("XL",40),
    ("X", 10),
    ("IX", 9),
    ("V", 5),
    ("IV", 4),
    ("I", 1)
  )
}

