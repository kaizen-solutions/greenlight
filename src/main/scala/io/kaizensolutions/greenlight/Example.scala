package io.kaizensolutions.greenlight

import cats.data.{ Ior, NonEmptyChain }

object Usage {
  def main(args: Array[String]): Unit = {
    import GValidation._
    val validX = validate((s: String) => Ior.bothNec(s"String too short", s))
    val end: GValidation[String, NonEmptyChain[String], String *** String *** String *** String] =
      validX and validX and validX and validX

//    end.map {
//      case a *** b *** c *** d => ???
//    }

    //
    //    val validA = validate((s: String) => Ior.bothNec(s"String too long", ()))

    val validY: GValidation[String, NonEmptyChain[String], (String, Int)] =
      ((s: String) => Ior.leftNec(s"String is too low")).lift

    val validZ: GValidation[(String, Int), NonEmptyChain[String], (String, Int)] =
      validate((input: (String, Int)) => Ior.bothNec("String and Int", input))

    validY >>> validZ
    //
    //    val example: GValidation[String, NonEmptyChain[String], ((String, (String, Int)), String)] =
    //      (validX + (validY >>> validZ)) + validA.preserveInput
    //
    //    println {
    //      example.run("1")
    //    }
  }
}
