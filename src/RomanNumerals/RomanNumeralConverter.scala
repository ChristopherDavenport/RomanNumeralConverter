package RomanNumerals

/**
  * Created by davenpcm on 2/5/2016.
  * Taken from and modified by several sources online.
  */
class RomanNumeralConverter {

  /**
    * Public method taking an Int and Returns a the string of Roman Numerals
    */
   def convertToRoman(number: Int): String = {
     if (number <= 0) "Invalid number"
     else toRomanNumerals( number, RomanNumerals)
  }

  /**
    * Public Method taking a string of Roman Numeral and Converts to an Int
    */
  def convertFromRoman(numeral: String): Int = {
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
    * There should be a way to do this utilizing Roman numerals as the base but I couldn't figure it out. So I wrote
    * it so it works. But it is repetitive. If you know a fix please let me know.
    */
  private def fromRomanNumerals(numeral: String, digits: List[(String, Int)]): Int = numeral match {
    case n if n.startsWith("M") => 1000 + fromRomanNumerals(numeral.substring(1), RomanNumerals)
    case n if n.startsWith("CM") => 900 + fromRomanNumerals(numeral.substring(2), RomanNumerals)
    case n if n.startsWith("D") => 500 + fromRomanNumerals(numeral.substring(1), RomanNumerals)
    case n if n.startsWith("CD") => 400 + fromRomanNumerals(numeral.substring(2), RomanNumerals)
    case n if n.startsWith("C") => 100 + fromRomanNumerals(numeral.substring(1), RomanNumerals)
    case n if n.startsWith("XC") => 90 + fromRomanNumerals(numeral.substring(2), RomanNumerals)
    case n if n.startsWith("L") => 50 + fromRomanNumerals(numeral.substring(1), RomanNumerals)
    case n if n.startsWith("XL") => 40 + fromRomanNumerals(numeral.substring(2), RomanNumerals)
    case n if n.startsWith("X") => 10 + fromRomanNumerals(numeral.substring(1), RomanNumerals)
    case n if n.startsWith("IX") => 9 + fromRomanNumerals(numeral.substring(2), RomanNumerals)
    case n if n.startsWith("V") => 5 + fromRomanNumerals(numeral.substring(1), RomanNumerals)
    case n if n.startsWith("IV") => 10 + fromRomanNumerals(numeral.substring(2), RomanNumerals)
    case n if n.startsWith("I") => 1 + fromRomanNumerals(numeral.substring(1), RomanNumerals)
    case _ => 0
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

