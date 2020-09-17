package io.kaizensolutions.calv2

import scala.util.Try

object InterestingExample extends App {
  import Validator._
  val s1 = fromPredicate[String](_ == "ABC")
    .asErrorWithInput((i: String) => s"String is not ABC (got $i)")
    .asWarning("HELLO")
    .exposeCause

  val s2 =
    fromTry[String, Int](s => Try(s.toInt))
      .asError("String is not a number")
      .asWarning("BOOM")
      .exposeCause

  println(s1.run("ABC"))
  println(s2.run("ABC"))

  println {
    (s1 andThen s2).run("ABC")
  }
}
