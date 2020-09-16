package io.kaizensolutions.calv2

import scala.util.Try

final case class Validator[-I, +E, +W, +A](run: I => Result[E, W, A]) { self =>
  def zipWith[I1 <: I, E1 >: E, W1 >: W: Combine, B, C](that: Validator[I1, E1, W1, B])(
    f: (A, B) => C
  ): Validator[I1, E1, W1, C] = Validator { i =>
    val resA = self.run(i)
    val resB = that.run(i)
    resA.zipWith(resB)(f)
  }

  def zipRight[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, B] =
    zipWith(that)((_, b) => b)

  def zipLeft[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, A] =
    zipWith(that)((a, _) => a)

  def and[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, (A, B)] =
    zipWith(that)((_, _))

  def or[I1 <: I, E1 >: E, W1 >: W, A1 >: A](that: Validator[I1, E1, W1, A1]): Validator[I1, E1, W1, A1] =
    Validator { i =>
      val first = self.run(i)
      if (first.succeeded) first
      else that.run(i)
    }

  def fallback[I1 <: I, E1 >: E, W1 >: W, A1 >: A](that: Validator[I1, E1, W1, A1]): Validator[I1, E1, W1, A1] =
    or(that)

  def andThen[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[A, E1, W1, B]): Validator[I1, E1, W1, B] =
    Validator(i =>
      self.run(i) match {
        case Result.Success(result)                     => that.run(result)
        case sw @ Result.SuccessWithWarnings(_, result) => sw *> that.run(result)
        case e @ Result.Error(_)                        => e
        case ew @ Result.ErrorWithWarnings(_, _)        => ew
      }
    )

  def flatMap[I1 <: I, E1 >: E, W1 >: W: Combine, B](f: A => Validator[I1, E1, W1, B]): Validator[I1, E1, W1, B] =
    Validator { i =>
      self.run(i) match {
        case Result.Success(result)                     => f(result).run(i)
        case sw @ Result.SuccessWithWarnings(_, result) => sw *> f(result).run(i)
        case e @ Result.Error(_)                        => e
        case ew @ Result.ErrorWithWarnings(_, _)        => ew
      }
    }

  def map[B](f: A => B): Validator[I, E, W, B] = Validator(i => self.run(i).mapResult(f))

  def mapResult[B](f: A => B): Validator[I, E, W, B] = map(f)

  def asResult[B](b: B): Validator[I, E, W, B] = mapResult(_ => b)

  def mapError[E1](f: E => E1): Validator[I, E1, W, A] = Validator(i => self.run(i).mapError(f))

  def mapErrorWithInput[I1 <: I, E1](f: (I1, E) => E1): Validator[I1, E1, W, A] =
    Validator { i =>
      self.run(i) match {
        case s @ Result.Success(_)                   => s
        case sw @ Result.SuccessWithWarnings(_, _)   => sw
        case Result.Error(error)                     => Result.Error(error = f(i, error))
        case ew @ Result.ErrorWithWarnings(_, error) => ew.copy(error = f(i, error))
      }
    }

  def asError[E1](e: E1): Validator[I, E1, W, A] = mapError(_ => e)

  def asErrorWithInput[I1 <: I, E1](f: I1 => E1): Validator[I1, E1, W, A] = mapErrorWithInput((i, _) => f(i))

  def mapWarning[WW](f: W => WW): Validator[I, E, WW, A] = Validator(i => self.run(i).mapWarning(f))

  def asWarning[W1](w: W1): Validator[I, E, W1, A] = Validator(i =>
    self.run(i) match {
      case Result.Success(result)                => Result.SuccessWithWarnings(w, result)
      case Result.SuccessWithWarnings(_, result) => Result.SuccessWithWarnings(w, result)
      case Result.Error(error)                   => Result.ErrorWithWarnings(w, error)
      case Result.ErrorWithWarnings(_, error)    => Result.ErrorWithWarnings(w, error)
    }
  )

  def addWarning[WW >: W: Combine](w: WW): Validator[I, E, WW, A] = Validator(i => self.run(i).appendWarning(w))

  def contramapInput[II](f: II => I): Validator[II, E, W, A] = Validator(ii => self.run(f(ii)))

  def exposeCause[I1 <: I, W1 >: W, E1 >: E, A1 >: A]: ValidatorCause[I, E, W, A] =
    ValidatorCause(self.mapError(Cause.single).mapWarning(Cause.single))

  def *>[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, B] =
    zipRight(that)

  def <*[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, B] =
    zipRight(that)

  def >>>[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[A, E1, W1, B]): Validator[I1, E1, W1, B] =
    andThen(that)

  def +[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, (A, B)] =
    and(that)

  def |[I1 <: I, E1 >: E, W1 >: W: Combine, A1 >: A](that: Validator[I1, E1, W1, A1]): Validator[I1, E1, W1, A1] =
    or(that)

  def >>=[I1 <: I, E1 >: E, W1 >: W: Combine, B](f: A => Validator[I1, E1, W1, B]): Validator[I1, E1, W1, B] =
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

  def fromPredicateWithError[I, E](p: I => Boolean)(error: E): Validator[I, E, Nothing, I] =
    Validator { i =>
      val res = p(i)
      if (res) Result.succeed(i)
      else Result.fail(error)
    }
}

case class ValidatorCause[-I, +E, +W, +A](underlying: Validator[I, Cause[E], Cause[W], A]) { self =>
  def run(i: I): Result[Cause[E], Cause[W], A] = underlying.run(i)

  def zipWith[I1 <: I, E1 >: E, W1 >: W, B, C](
    that: ValidatorCause[I1, E1, W1, B]
  )(f: (A, B) => C): ValidatorCause[I1, E1, W1, C] =
    ValidatorCause(underlying.zipWith(that.underlying)(f)(Cause.causeBothCombine))

  def zipRight[I1 <: I, E1 >: E, W1 >: W, B](
    that: ValidatorCause[I1, E1, W1, B]
  ): ValidatorCause[I1, E1, W1, B] =
    ValidatorCause(underlying.zipRight(that.underlying)(Cause.causeBothCombine))

  def zipLeft[I1 <: I, E1 >: E, W1 >: W, B](
    that: ValidatorCause[I1, E1, W1, B]
  ): ValidatorCause[I1, E1, W1, A] =
    ValidatorCause(underlying.zipLeft(that.underlying)(Cause.causeBothCombine))

  def and[I1 <: I, E1 >: E, W1 >: W, B](
    that: ValidatorCause[I1, E1, W1, B]
  ): ValidatorCause[I1, E1, W1, (A, B)] =
    ValidatorCause(underlying.and(that.underlying)(Cause.causeBothCombine))

  def or[I1 <: I, E1 >: E, W1 >: W, A1 >: A](
    that: ValidatorCause[I1, E1, W1, A1]
  ): ValidatorCause[I1, E1, W1, A1] = ValidatorCause(Validator { i =>
    val first = underlying.run(i)
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
  })

  def fallback[I1 <: I, E1 >: E, W1 >: W, A1 >: A](
    that: ValidatorCause[I1, E1, W1, A1]
  ): ValidatorCause[I1, E1, W1, A1] = self.or(that)

  def flatMap[I1 <: I, E1 >: E, W1 >: W, B](
    f: A => ValidatorCause[I1, E1, W1, B]
  ): ValidatorCause[I1, E1, W1, B] =
    ValidatorCause(underlying.flatMap(a => f(a).underlying)(Cause.causeThenCombine))

  def andThen[I1 <: I, E1 >: E, W1 >: W, B](
    that: ValidatorCause[A, E1, W1, B]
  ): ValidatorCause[I1, E1, W1, B] =
    ValidatorCause(
      Validator(i =>
        self.run(i) match {
          case Result.Success(result) =>
            that.run(result)

          case sw @ Result.SuccessWithWarnings(_, result) =>
            sw.zipWith(that.run(result))((_, r) => r)(Cause.causeThenCombine)

          case e @ Result.Error(_) =>
            e

          case ew @ Result.ErrorWithWarnings(_, _) =>
            ew
        }
      )
    )
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
