package io.kaizensolutions.calv1

import scala.util.Try

final case class Validator[-I, +E, +W, +A](run: I => Result[E, W, A]) { self =>
  def zipWith[I1 <: I, E1 >: E, W1 >: W, B, C](that: Validator[I1, E1, W1, B])(
    f: (A, B) => C
  ): Validator[I1, E1, W1, C] = Validator { i =>
    val resA = self.run(i)
    val resB = that.run(i)
    resA.zipWith(resB)(f)
  }

  def zipRight[I1 <: I, E1 >: E, W1 >: W, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, B] =
    zipWith(that)((_, b) => b)

  def zipLeft[I1 <: I, E1 >: E, W1 >: W, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, A] =
    zipWith(that)((a, _) => a)

  def and[I1 <: I, E1 >: E, W1 >: W, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, (A, B)] =
    zipWith(that)((_, _))

  def or[I1 <: I, E1 >: E, W1 >: W, A1 >: A](that: Validator[I1, E1, W1, A1]): Validator[I1, E1, W1, A1] =
    Validator { i =>
      val first = self.run(i)
      if (first.succeeded) first
      else that.run(i)
    }

  def fallback[I1 <: I, E1 >: E, W1 >: W, A1 >: A](that: Validator[I1, E1, W1, A1]): Validator[I1, E1, W1, A1] =
    or(that)

  def andThen[I1 <: I, E1 >: E, W1 >: W, B](that: Validator[A, E1, W1, B]): Validator[I1, E1, W1, B] =
    Validator(i =>
      self.run(i) match {
        case Result.Success(warnings, result) => that.run(result).prependWarnings(warnings)
        case Result.Error(warnings, error)    => Result.Error(warnings, error)
      }
    )

  def flatMap[I1 <: I, E1 >: E, W1 >: W, B](f: A => Validator[I1, E1, W1, B]): Validator[I1, E1, W1, B] =
    Validator { i =>
      self.run(i) match {
        case Result.Success(warnings, result) =>
          f(result).run(i).prependWarnings(warnings)

        case e @ Result.Error(_, _) =>
          e
      }
    }

  def map[B](f: A => B): Validator[I, E, W, B] = Validator(i => self.run(i).mapResult(f))

  def mapResult[B](f: A => B): Validator[I, E, W, B] = map(f)

  def asResult[B](b: B): Validator[I, E, W, B] = mapResult(_ => b)

  def mapError[E1](f: E => E1): Validator[I, E1, W, A] = Validator(i => self.run(i).mapError(f))

  def mapErrorWithInput[I1 <: I, E1](f: (I1, E) => E1): Validator[I1, E1, W, A] =
    Validator { i =>
      self.run(i) match {
        case Result.Success(warnings, result) => Result.Success(warnings, result)
        case Result.Error(warnings, error)    => Result.Error(warnings, f(i, error))
      }
    }

  def asError[E1](e: E1): Validator[I, E1, W, A] = mapError(_ => e)

  def asErrorWithInput[I1 <: I, E1](f: I1 => E1): Validator[I1, E1, W, A] = mapErrorWithInput((i, _) => f(i))

  def mapWarnings[WW](f: W => WW): Validator[I, E, WW, A] = Validator(i => self.run(i).mapWarnings(f))

  def withWarning[WW >: W](w: WW): Validator[I, E, WW, A] = Validator(i => self.run(i).appendWarning(w))

  def withWarnings[WW >: W](ws: Vector[WW]): Validator[I, E, WW, A] = Validator(i => self.run(i).appendWarnings(ws))

  def contramapInput[II](f: II => I): Validator[II, E, W, A] = Validator(ii => self.run(f(ii)))

  def *>[I1 <: I, E1 >: E, W1 >: W, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, B] =
    zipRight(that)

  def <*[I1 <: I, E1 >: E, W1 >: W, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, B] =
    zipRight(that)

  def >>>[I1 <: I, E1 >: E, W1 >: W, B](that: Validator[A, E1, W1, B]): Validator[I1, E1, W1, B] =
    andThen(that)

  def +[I1 <: I, E1 >: E, W1 >: W, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, (A, B)] =
    and(that)

  def |[I1 <: I, E1 >: E, W1 >: W, A1 >: A](that: Validator[I1, E1, W1, A1]): Validator[I1, E1, W1, A1] =
    or(that)

  def >>=[I1 <: I, E1 >: E, W1 >: W, B](f: A => Validator[I1, E1, W1, B]): Validator[I1, E1, W1, B] =
    flatMap(f)
}

object Validator {
  def pure[I]: Validator[I, Nothing, Nothing, I] =
    Validator(i => Result.succeed(i))

  def lift[A](a: A): Validator[Any, Nothing, Nothing, A] =
    Validator(_ => Result.succeed(a))

  def fromFunction[I, O](fn: I => O): Validator[I, Nothing, Nothing, O] =
    Validator(i => Result.succeed(fn(i)))

  def fromEither[I, E, O](fn: I => Either[E, O]): Validator[I, E, Nothing, O] =
    Validator(i => fn(i).fold(Result.fail, Result.succeed))

  def fromTry[I, O](fn: I => Try[O]): Validator[I, Throwable, Nothing, O] =
    Validator(i => fn(i).fold(Result.fail, Result.succeed))

  def fromPredicate[I](p: I => Boolean): Validator[I, Unit, Nothing, I] =
    Validator { i =>
      val res = p(i)
      if (res) Result.succeed(i)
      else Result.fail(())
    }
}

trait ValidatorSyntax {
  def validate[I]: Validator[I, Nothing, Nothing, I] = Validator.pure[I]

  implicit class ValidatorValueOps[A](a: A) {
    def validate: Validator[Any, Nothing, Nothing, A] =
      Validator.lift(a)
  }

  implicit class ValidatorFunctionOps[I, O](fn: Function[I, O]) {
    def toValidator: Validator[I, Nothing, Nothing, O] = Validator.fromFunction(fn)
  }

  implicit class ValidatorPredicateOps[I, O](fn: Function[I, Boolean]) {
    def toValidator: Validator[I, Unit, Nothing, I] = Validator.fromPredicate(fn)
  }

  implicit class ValidatorTryOps[I, O](fn: Function[I, Try[O]]) {
    def toValidator: Validator[I, Throwable, Nothing, O] = Validator.fromTry(fn)
  }

  implicit class ValidatorEitherOps[I, E, O](fn: Function[I, Either[E, O]]) {
    def toValidator: Validator[I, E, Nothing, O] = Validator.fromEither(fn)
  }
}
