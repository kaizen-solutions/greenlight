package io.kaizensolutions.calv2

import scala.util.Try

object InterestingExample extends App {
  implicit class ValidatorCauseOps[-I, +E, +W, +A](self: Validator[I, Cause[E], Cause[W], A]) {
    def zipWith[I1 <: I, E1 >: E, W1 >: W, B, C](
      that: Validator[I1, Cause[E1], Cause[W1], B]
    )(f: (A, B) => C): Validator[I1, Cause[E1], Cause[W1], C] =
      self.zipWith(that)(f)(Cause.causeBothCombine)

    def zipRight[I1 <: I, E1 >: E, W1 >: W, B](
      that: Validator[I1, Cause[E1], Cause[W1], B]
    ): Validator[I1, Cause[E1], Cause[W1], B] =
      self.zipRight(that)(Cause.causeBothCombine)

    def zipLeft[I1 <: I, E1 >: E, W1 >: W, B](
      that: Validator[I1, Cause[E1], Cause[W1], B]
    ): Validator[I1, Cause[E1], Cause[W1], A] =
      self.zipLeft(that)(Cause.causeBothCombine)

    def and[I1 <: I, E1 >: E, W1 >: W, B](
      that: Validator[I1, Cause[E1], Cause[W1], B]
    ): Validator[I1, Cause[E1], Cause[W1], (A, B)] =
      self.and(that)(Cause.causeBothCombine)

    def or[I1 <: I, E1 >: E, W1 >: W, A1 >: A](
      that: Validator[I1, Cause[E1], Cause[W1], A1]
    ): Validator[I1, Cause[E1], Cause[W1], A1] = Validator { i =>
      val first = self.run(i)
      if (first.succeeded) first
      else {
        val fallback = that.run(i)
        fallback
          .mapWarning(fallbackW =>
            first.extractWarnings match {
              case Some(firstW) => Cause.fallback(firstW, fallbackW)
              case None         => fallbackW
            }
          )
          .mapError(fallbackE =>
            first.extractError match {
              case Some(firstE) => Cause.fallback(firstE, fallbackE)
              case None         => fallbackE
            }
          )
      }
    }
  }

  import Validator._
  val s1 = fromPredicate[String](_ == "ABC").asErrorWithInput((i: String) => s"String is not ABC (got $i)")
  val s2 = fromTry[String, Int](s => Try(s.toInt)).asError("String is not a number")

  println {
    (s1 fallback s2).run("rekt")
  }
}
