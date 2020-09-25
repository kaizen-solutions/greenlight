package io.kaizensolutions.calv4initial

import scala.util.Try
import ValidatorTrace._

object Example extends App {
  val abc: Validator[String, List[String], Nothing, String] =
    Validator.Pure { (s: String) =>
      if (s == "ABC") Result.Success(None, s)
      else Result.Error(None, List("BOOM"))
    }

  val convertToInt: Validator[String, List[String], Nothing, Int] =
    Validator.Pure((i: String) => Result.Success[Nothing, Int](None, i.toInt))

  val isInt: Validator[String, List[String], Nothing, String] = Validator.Pure { (s: String) =>
    Try(s.toInt)
      .fold(
        _ => Result.Error[Nothing, List[String]](None, List(s"$s is not a valid number")),
        (_: Int) => Result.Success[Nothing, String](None, s)
      )
  }

  println {
    abc
      .map(_ => 1337)
      .fallback(isInt.andThen(convertToInt))
      .trace("LOL")
  }
}
