object Main {
  def main(args: Array[String]): Unit = {
    println(Polycalculate.solves_a_polynome("(((2+3)*2)/(2+3))^2"))

    println(Polycalculate.solves_a_polynome("cos90"))







  }
}




import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{ break, breakable }

class Polycalculate() {

}

object Polycalculate {

  //***********************************************************************************************
  //  MAIN FONCTION : solves a polynomial or equation from a string
  //***********************************************************************************************
  def solves_a_polynome(polynomial: String): Double = {

    var polynome = polynomial

    // 1. I delete all empty spaces.
    polynome = polynome.replaceAll("\\s", "")


    // 2. I add an empty space between all the expressions
    polynome = polynome.replaceAll("\\(", " ( ")
    polynome = polynome.replaceAll("\\+", " + ")
    polynome = polynome.replaceAll("\\-", " - ")
    polynome = polynome.replaceAll("\\*", " * ")
    polynome = polynome.replaceAll("\\/", " / ")
    polynome = polynome.replaceAll("\\)", " ) ")

    polynome = polynome.replaceAll("T", " t ")
    polynome = polynome.replaceAll("t", " t ")

    polynome = polynome.replaceAll("R", " r ")
    polynome = polynome.replaceAll("r", " r ")

    polynome = polynome.replaceAll("O", " o ")
    polynome = polynome.replaceAll("o", " o ")

    polynome = polynome.replaceAll("B", " b ")
    polynome = polynome.replaceAll("b", " b ")

    polynome = polynome.replaceAll("D", " d ")
    polynome = polynome.replaceAll("d", " d ")

    polynome = polynome.replaceAll("F", " f ")
    polynome = polynome.replaceAll("f", " f ")

    polynome = polynome.replaceAll("H", " h ")
    polynome = polynome.replaceAll("h", " h ")

    polynome = polynome.replaceAll("L", " l ")
    polynome = polynome.replaceAll("l", " l ")

    polynome = polynome.replaceAll("EXP", " EXP ")
    polynome = polynome.replaceAll("exp", " exp ")
    polynome = polynome.replaceAll(" l  o G", " LOG ")
    polynome = polynome.replaceAll(" l  o g", " log ")
    polynome = polynome.replaceAll("SIN", " SIN ")
    polynome = polynome.replaceAll("sin", " sin ")
    polynome = polynome.replaceAll("C o S", " cos ")
    polynome = polynome.replaceAll("c o s", " cos ")

    polynome = polynome.replaceAll(" t AN", " tan ")
    polynome = polynome.replaceAll(" t an", " tan ")
    polynome = polynome.replaceAll("\\^", " ^ ")
    polynome = polynome.replaceAll("SQ r T ", " sqrt ")
    polynome = polynome.replaceAll("sq r t ", " sqrt ")
    polynome = polynome.replaceAll("C b  r  t ", " cbrt ")
    polynome = polynome.replaceAll("c b  r  t ", " cbrt ")

    polynome = polynome.replaceAll("PI", " pi ")
    polynome = polynome.replaceAll("pi", " pi ")

    // I avoid double empty spaces
    polynome = polynome.replaceAll("   ", " ")
    polynome = polynome.replaceAll("  ", " ")

    // 3. I transform the sting into a chain of expression.
    var list: ListBuffer[String] = ListBuffer.empty
    list ++= polynome.split(" ").map(_.trim).toList

    // I suppress the first space if it is empty. It can happen if the polynomial starts with a parenthesis.
    if (list.head.equals(""))
      list.remove(0)

    //4. I resolved what is in the brackets.
    // println(" ")
    // println("the polynomial  : " + list.mkString(" "))
    dissociate_parentheses_and_calculate(list)

    //5. Then the rest.
    // println(" ")
    // println("the final polynomial without the brackets  : " + list.mkString(" "))
    calculate(list)
    // And I return the last remaining value which must be the answer .
    // println(" ")

    // println("final result  : " + list.mkString(" "))
    list.head.toDouble

  }



  //***********************************************************************************************
  // This function will resolve the contents of the parentheses of the polynomial.
  //***********************************************************************************************
  private def dissociate_parentheses_and_calculate(list_first: ListBuffer[String]): ListBuffer[String] = {

    var new_list: ListBuffer[String] = ListBuffer.empty
    var nb = 0

    var index = -1
    var index_start = 0
    var index_stop = 0

    breakable {

      for (l <- list_first) {

        index = index + 1

        if (l.equals(")")) {
          //println(") detected")

          nb = nb - 1

          if (nb == 0) {
            //println("new liste : "+new_list)
            index_stop = index

            if (new_list.contains("(")) //  restart process
              dissociate_parentheses_and_calculate(new_list)

            //println("calciulus : "+calculus(new_list).toString())
            list_first(index_start) = calculate(new_list).head.toString()

            var a: Int = 0
            for (a <- (index_start + 1) to index_stop) {
              // println("remove    index : "+(index_start + 1))
              list_first.remove(index_start + 1)

            }

            //println("temporary results : " + list_first.mkString(" "))
            //
            //println(" ")

            dissociate_parentheses_and_calculate(list_first)
            break()

          }

        }

        if (nb > 0) {
          new_list = new_list += l

          // println("new liste : "+new_list)

          //println(l +"    nb : "+nb)
        }

        if (l.equals("(")) {
          // println("( detected")

          if (nb == 0)
            index_start = index

          nb = nb + 1

        }

        // println("index_start : "+index_start)
        // println("index_stop : "+index_stop)
        // println("index : "+index)
        // println("nb : "+nb)

      }
    }

    new_list
  }

  //***********************************************************************************************
  // Solves a polynomial/equation without parentheses.
  // It uses the mathematical expressions below (in the next fonctions).
  //***********************************************************************************************
  private def calculate(calculus_list: ListBuffer[String]): ListBuffer[String] = {



    if (calculus_list.contains("cos") || calculus_list.contains("COS"))
      cos(calculus_list)
    if (calculus_list.contains("sin") || calculus_list.contains("SIN"))
      sin(calculus_list)
    if (calculus_list.contains("tan") || calculus_list.contains("TAN"))
      tan(calculus_list)
    if (calculus_list.contains("sqrt") || calculus_list.contains("SQRT"))
      tan(calculus_list)
    if (calculus_list.contains("cbrt") || calculus_list.contains("CBRT"))
      tan(calculus_list)
    if (calculus_list.contains("^"))
      power(calculus_list)
    if (calculus_list.contains("log") || calculus_list.contains("LOG"))
      log(calculus_list)
    if (calculus_list.contains("exp") || calculus_list.contains("EXP"))
      exp(calculus_list)
    if (calculus_list.contains("*"))
      multiplication(calculus_list)
    if (calculus_list.contains("/"))
      division(calculus_list)
    if (calculus_list.contains("+"))
      addition(calculus_list)
    if (calculus_list.contains("-"))
      subtraction(calculus_list)

    calculus_list
  }

  //***********************************************************************************************
  //
  //   mathematical expressions
  //      Basic operations :                  + - * /
  //      Logarithm and exponential :         exp-EXP log-LOG
  //      Power, square root and cube root :  ^ sqrt-SQRL cbrt-CBRT
  //      Trigonometry :                      sin-SIN cos-COS tan-TAN
  //
  //   External input (sensors)
  //      T or t for temperature sensor
  //      R or r for humidity sensor
  //
  //   Mathematical constants
  //      PI or pi for Math.PI
  //      E or e   for Math.E
  //
  //***********************************************************************************************

  // Basic operations

  private def addition(calculus_list_add: ListBuffer[String]): ListBuffer[String] = {
    // println("calculate addition : " + calculus_list_add.mkString(" "))

    var index = -1

    breakable {
      for (l <- calculus_list_add) {

        index = index + 1
        // println(index)
        if (l.equals("+")) {

          if (index == 0) {

            calculus_list_add.remove(index)

            if (calculus_list_add.contains("+")) {

              addition(calculus_list_add)
            }

            //      println("----------------------break------------------------")
            break()

          } else if (testDouble(calculus_list_add(index - 1))) {

            val newvalue = calculus_list_add(index - 1).toDouble + calculus_list_add(index + 1).toDouble
            calculus_list_add(index) = newvalue.toString
            calculus_list_add.remove(index - 1)
            calculus_list_add.remove(index)
            if (calculus_list_add.contains("+")) {

              addition(calculus_list_add)
            }

            //      println("----------------------break------------------------")
            break()

          } else {

            calculus_list_add.remove(index)

            if (calculus_list_add.contains("+")) {

              addition(calculus_list_add)
            }

            //      println("----------------------break------------------------")
            break()

          }

        }
      }
    }
    calculus_list_add
  }

  private def subtraction(calculus_list_add: ListBuffer[String]): ListBuffer[String] = {
    // println("calculate subtraction : " + calculus_list_add.toString())

    var index = -1

    breakable {
      for (l <- calculus_list_add) {

        index = index + 1
        // println(index)
        if (l.equals("-")) {

          if (index == 0) {

            calculus_list_add.remove(index)
            calculus_list_add(index) = (-calculus_list_add(index).toDouble).toString

            if (calculus_list_add.contains("-")) {

              subtraction(calculus_list_add)
            }

            //      println("----------------------break------------------------")
            break()

          } else if (testDouble(calculus_list_add(index - 1))) {

            val newvalue = calculus_list_add(index - 1).toDouble - calculus_list_add(index + 1).toDouble
            calculus_list_add(index) = newvalue.toString
            calculus_list_add.remove(index - 1)
            calculus_list_add.remove(index)

            if (calculus_list_add.contains("-")) {

              subtraction(calculus_list_add)
            }

            //      println("----------------------break------------------------")
            break()
          } else {

            calculus_list_add.remove(index)
            calculus_list_add(index) = (-calculus_list_add.remove(index).toDouble).toString

            if (calculus_list_add.contains("-")) {

              subtraction(calculus_list_add)
            }

            //      println("----------------------break------------------------")
            break()

          }
        }

      }
    }
    calculus_list_add
  }

  private def multiplication(calculus_list_add: ListBuffer[String]): ListBuffer[String] = {
    // println("calculate multiplication : " + calculus_list_add.toString())

    //val bool = false

    var index = -1
    breakable {
      for (l <- calculus_list_add) {
        index = index + 1
        // println(index)

        if (l.equals("*")) {

          val newvalue = calculus_list_add(index - 1).toDouble * calculus_list_add(index + 1).toDouble
          calculus_list_add(index) = newvalue.toString

          calculus_list_add.remove(index - 1)
          calculus_list_add.remove(index)

          if (calculus_list_add.contains("*")) {

            multiplication(calculus_list_add)
          }

          //      println("----------------------break------------------------")
          break()
        }

      }
    }
    calculus_list_add
  }

  private def division(calculus_list_add: ListBuffer[String]): ListBuffer[String] = {
    //println("calculates division : " + calculus_list_add.toString())

    //val bool = false

    var index = -1
    breakable {
      for (l <- calculus_list_add) {
        index = index + 1
        // println(index)

        if (l.equals("/")) {

          val newvalue = calculus_list_add(index - 1).toDouble / calculus_list_add(index + 1).toDouble
          calculus_list_add(index) = newvalue.toString

          calculus_list_add.remove(index - 1)
          calculus_list_add.remove(index)

          if (calculus_list_add.contains("/")) {

            division(calculus_list_add)
          }

          //      println("----------------------break------------------------")
          break()
        }

      }
    }
    calculus_list_add
  }

  // Logarithm and exponential

  private def exp(calculus_list_add: ListBuffer[String]): ListBuffer[String] = {
    // println("calculates exp : " + calculus_list_add.toString())

    //val bool = false

    var index = -1
    breakable {
      for (l <- calculus_list_add) {
        index = index + 1
        // println(index)

        if (l.equals("exp") || l.equals("EXP")) {

          val newvalue = java.lang.Math.exp(calculus_list_add(index + 1).toDouble)
          calculus_list_add(index) = newvalue.toString

          calculus_list_add.remove(index + 1)

          if (calculus_list_add.contains("exp") || calculus_list_add.contains("EXP")) {

            exp(calculus_list_add)
          }

          //      println("----------------------break------------------------")
          break()
        }

      }
    }
    calculus_list_add
  }

  private def log(calculus_list_add: ListBuffer[String]): ListBuffer[String] = {
    //println("calculates log : " + calculus_list_add.toString())

    //val bool = false

    var index = -1
    breakable {
      for (l <- calculus_list_add) {
        index = index + 1
        // println(index)

        if (l.equals("log") || l.equals("LOG")) {

          val newvalue = java.lang.Math.log(calculus_list_add(index + 1).toDouble)
          calculus_list_add(index) = newvalue.toString

          calculus_list_add.remove(index + 1)

          if (calculus_list_add.contains("log") || calculus_list_add.contains("LOG")) {

            log(calculus_list_add)
          }

          //      println("----------------------break------------------------")
          break()
        }

      }
    }
    calculus_list_add
  }

  // Power, square root and cube root

  private def power(calculus_list_add: ListBuffer[String]): ListBuffer[String] = {
    //println("calculate power : " + calculus_list_add.mkString(" "))

    var index = -1
    breakable {
      for (l <- calculus_list_add) {
        index = index + 1
        // println(index)

        if (l.equals("^")) {

          val newvalue = Math.pow(calculus_list_add(index - 1).toDouble, calculus_list_add(index + 1).toDouble)
          calculus_list_add(index) = newvalue.toString

          calculus_list_add.remove(index - 1)
          calculus_list_add.remove(index)

          if (calculus_list_add.contains("^")) {

            power(calculus_list_add)
          }

          //      println("----------------------break------------------------")
          break()
        }

      }
    }
    calculus_list_add
  }

  private def sqrt(calculus_list_add: ListBuffer[String]): ListBuffer[String] = {
    // println("calculate square root : " + calculus_list_add.mkString(" "))

    //val bool = false

    var index = -1
    breakable {
      for (l <- calculus_list_add) {
        index = index + 1
        // println(index)

        if (l.equals("sqrt") || l.equals("SQRT")) {

          val newvalue = java.lang.Math.sqrt(calculus_list_add(index + 1).toDouble)
          calculus_list_add(index) = newvalue.toString

          calculus_list_add.remove(index + 1)

          if (calculus_list_add.contains("sqrt") || calculus_list_add.contains("SQRT")) {

            sqrt(calculus_list_add)
          }

          //      println("----------------------break------------------------")
          break()
        }

      }
    }
    calculus_list_add
  }

  private def cbrt(calculus_list_add: ListBuffer[String]): ListBuffer[String] = {
    //println("calculate cube root : " + calculus_list_add.mkString(" "))

    //val bool = false

    var index = -1
    breakable {
      for (l <- calculus_list_add) {
        index = index + 1
        // println(index)

        if (l.equals("cbrt") || l.equals("CBRT")) {

          val newvalue = java.lang.Math.cbrt(calculus_list_add(index + 1).toDouble)
          calculus_list_add(index) = newvalue.toString

          calculus_list_add.remove(index + 1)

          if (calculus_list_add.contains("cbrt") || calculus_list_add.contains("CBRT")) {

            cbrt(calculus_list_add)
          }

          //      println("----------------------break------------------------")
          break()
        }

      }
    }
    calculus_list_add
  }

  // Trigonometry

  private def sin(calculus_list_add: ListBuffer[String]): ListBuffer[String] = {
    // println("calculate sin : " + calculus_list_add.mkString(" "))

    //val bool = false

    var index = -1
    breakable {
      for (l <- calculus_list_add) {
        index = index + 1
        // println(index)

        if (l.equals("sin") || l.equals("SIN")) {

          val newvalue = java.lang.Math.sin(calculus_list_add(index + 1).toDouble)
          calculus_list_add(index) = newvalue.toString

          calculus_list_add.remove(index + 1)

          if (calculus_list_add.contains("sin") || calculus_list_add.contains("SIN")) {

            sin(calculus_list_add)
          }

          //      println("----------------------break------------------------")
          break()
        }

      }
    }
    calculus_list_add
  }

  private def cos(calculus_list_add: ListBuffer[String]): ListBuffer[String] = {
    // println("calculate cos : " + calculus_list_add.mkString(" "))

    //val bool = false

    var index = -1
    breakable {
      for (l <- calculus_list_add) {
        index = index + 1
        // println(index)

        if (l.equals("cos") || l.equals("COS")) {

          val newvalue = java.lang.Math.cos(calculus_list_add(index + 1).toDouble)
          calculus_list_add(index) = newvalue.toString

          calculus_list_add.remove(index + 1)

          if (calculus_list_add.contains("cos") || calculus_list_add.contains("COS")) {

            cos(calculus_list_add)
          }

          //      println("----------------------break------------------------")
          break()
        }

      }
    }
    calculus_list_add
  }

  private def tan(calculus_list_add: ListBuffer[String]): ListBuffer[String] = {
    // println("calculate tan : " + calculus_list_add.mkString(" "))

    //val bool = false

    var index = -1
    breakable {
      for (l <- calculus_list_add) {
        index = index + 1
        // println(index)

        if (l.equals("tan") || l.equals("TAN")) {

          val newvalue = java.lang.Math.tan(calculus_list_add(index + 1).toDouble)
          calculus_list_add(index) = newvalue.toString

          calculus_list_add.remove(index + 1)

          if (calculus_list_add.contains("tan") || calculus_list_add.contains("TAN")) {

            tan(calculus_list_add)
          }

          //      println("----------------------break------------------------")
          break()
        }

      }
    }
    calculus_list_add
  }



  // Mathematical constants

  private def constants(calculus_list_add: ListBuffer[String]): ListBuffer[String] = {
    // println("give the corresponding value of the constant : " + calculus_list_add.toString())

    // -----> use a Kst or Khst Object

    var index = -1
    breakable {
      for (l <- calculus_list_add) {
        index = index + 1

        l match {

          //  --------- OK

          case "PI" => calculus_list_add(index) = Math.PI.toString
          case "pi" => calculus_list_add(index) = Math.PI.toString

          case "E"  => calculus_list_add(index) = Math.E.toString
          case "e"  => calculus_list_add(index) = Math.E.toString


        }

      }
    }
    calculus_list_add
  }

  //***********************************************************************************************
  // Fonctions to test if a String cold be a Double.
  //***********************************************************************************************
  def parseDouble(s: String): Option[Double] = try { Some(s.toDouble) } catch { case _ => None }
  def testDouble(s: String): Boolean = parseDouble(s).isDefined

}

