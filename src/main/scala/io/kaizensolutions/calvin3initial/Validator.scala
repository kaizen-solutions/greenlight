package io.kaizensolutions.calvin3initial

import shapeless.=:!=

sealed trait Validator[-I, +E, +W, +A] { self =>
  def zipWith[I1 <: I, E1 >: E, W1 >: W: Combine, B, C](that: Validator[I1, E1, W1, B])(
    f: (A, B) => C
  ): Validator[I1, E1, W1, C] = ???

  def zipRight[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, B] =
    zipWith(that)((_, b) => b)

  def zipLeft[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, A] =
    zipWith(that)((a, _) => a)

  def and[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, (A, B)] =
    zipWith(that)((_, _))

  def or[I1 <: I, E1 >: E, W1 >: W, A1 >: A](that: Validator[I1, E1, W1, A1]): Validator[I1, E1, W1, A1] = ???

  def fallback[I1 <: I, E1 >: E, W1 >: W, A1 >: A](that: Validator[I1, E1, W1, A1]): Validator[I1, E1, W1, A1] =
    or(that)

  def andThen[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[A, E1, W1, B]): Validator[I1, E1, W1, B] = ???

  def flatMap[I1 <: I, E1 >: E, W1 >: W: Combine, B](f: A => Validator[I1, E1, W1, B]): Validator[I1, E1, W1, B] = ???

  def map[B](f: A => B): Validator[I, E, W, B] = ???

  def mapResult[B](f: A => B): Validator[I, E, W, B] = map(f)

  def asResult[B](b: B): Validator[I, E, W, B] = mapResult(_ => b)

  def mapError[E1](f: E => E1): Validator[I, E1, W, A] = ???

  def mapErrorWithInput[I1 <: I, E1](f: (I1, E) => E1): Validator[I1, E1, W, A] = ???

  def asError[E1](e: E1): Validator[I, E1, W, A] = mapError(_ => e)

  def asErrorWithInput[I1 <: I, E1](f: I1 => E1): Validator[I1, E1, W, A] = mapErrorWithInput((i, _) => f(i))

  def mapWarning[WW](f: W => WW): Validator[I, E, WW, A] = ???

  def asWarning[W1](w: W1): Validator[I, E, W1, A] = ???

  def addWarning[WW >: W: Combine](w: WW): Validator[I, E, WW, A] = ???

  def contramapInput[II](f: II => I): Validator[II, E, W, A] = ???

  def exposeCause[I1 <: I, W1 >: W, E1 >: E, A1 >: A]: Validator[I, E, W, A] = ???

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

  def run(i: I): Result[E, W, A] = self match {
    case Validator.Pure(underlying) =>
      underlying(i)

    case Validator.MapResult(underlying, f) =>
      underlying.run(i).mapResult(f)

    case Validator.MapError(underlying, f) =>
      underlying.run(i).mapError(f)

    case Validator.MapWarning(underlying, f) =>
      underlying.run(i).mapWarning(f)

    case Validator.FlatMap(underlying, f, combineW) =>
      underlying.run(i) match {
        case Result.Success(result) =>
          f(result).run(i)

        case Result.SuccessWithWarnings(warnings, result) =>
          f(result).run(i) match {
            case Result.Success(result) =>
              Result.SuccessWithWarnings(warnings, result)

            case Result.SuccessWithWarnings(resultWarnings, result) =>
              Result.SuccessWithWarnings(combineW.combine(warnings, resultWarnings), result)

            case Result.Error(error) =>
              Result.ErrorWithWarnings(warnings, error)

            case Result.ErrorWithWarnings(resultWarnings, error) =>
              Result.ErrorWithWarnings(combineW.combine(warnings, resultWarnings), error)
          }

        case Result.Error(error) => Result.Error(error)

        case Result.ErrorWithWarnings(warnings, error) =>
          Result.ErrorWithWarnings(warnings, error)
      }

    case zipCase: Validator.ZipWith[I, E, W, A, b, c] =>
      val left  = zipCase.left.run(i)
      val right = zipCase.right.run(i)
      val f     = zipCase.f
      val fW    = zipCase.combineW.combine _
      val fE    = zipCase.combineE.combine _

      (left, right) match {
        case (Result.Success(a), Result.Success(b)) =>
          Result.Success(f(a, b))

        case (Result.Success(a), Result.SuccessWithWarnings(w, b)) =>
          Result.SuccessWithWarnings(w, f(a, b))

        case (Result.Success(a), Result.Error(b)) =>
          Result.Error(b)

        case (Result.Success(a), Result.ErrorWithWarnings(w, b)) =>
          Result.ErrorWithWarnings(w, b)

        case (Result.SuccessWithWarnings(w1, a), Result.Success(b)) =>
          Result.SuccessWithWarnings(w1, f(a, b))

        case (Result.SuccessWithWarnings(w1, a), Result.SuccessWithWarnings(w2, b)) =>
          Result.SuccessWithWarnings(fW(w1, w2), f(a, b))

        case (Result.SuccessWithWarnings(w1, a), Result.Error(e)) =>
          Result.ErrorWithWarnings(w1, e)

        case (Result.SuccessWithWarnings(w1, a), Result.ErrorWithWarnings(w2, e)) =>
          Result.ErrorWithWarnings(fW(w1, w2), e)

        case (Result.ErrorWithWarnings(w1, e1), Result.Success(b)) =>
          Result.ErrorWithWarnings(w1, e1)

        case (Result.ErrorWithWarnings(w1, e1), Result.Error(e2)) =>
          Result.ErrorWithWarnings(w1, fE(e1, e2))

        case (Result.ErrorWithWarnings(w1, e1), Result.SuccessWithWarnings(w2, b)) =>
          Result.ErrorWithWarnings(fW(w1, w2), e1)

        case (Result.ErrorWithWarnings(w1, e1), Result.ErrorWithWarnings(w2, e2)) =>
          Result.ErrorWithWarnings(fW(w1, w2), fE(e1, e2))

        case (Result.Error(e1), Result.Error(e2)) =>
          Result.Error(fE(e1, e2))

        case (Result.Error(e1), Result.Success(b)) =>
          Result.Error(e1)

        case (Result.Error(e1), Result.SuccessWithWarnings(w2, b)) =>
          Result.ErrorWithWarnings(w2, e1)

        case (Result.Error(e1), Result.ErrorWithWarnings(w2, e2)) =>
          Result.ErrorWithWarnings(w2, fE(e1, e2))
      }

    case Validator.Fallback(first, second, combineE, combineW) =>
      first.run(i) match {
        case Result.Success(result) =>
          Result.Success(result)

        case Result.SuccessWithWarnings(w1, result) =>
          Result.SuccessWithWarnings(w1, result)

        case Result.Error(e1) =>
          second.run(i) match {
            case Result.Success(result) =>
              Result.Success(result)

            case Result.SuccessWithWarnings(warnings, result) =>
              Result.SuccessWithWarnings(warnings, result)

            case Result.Error(e2) =>
              Result.Error(combineE.combine(e1, e2))

            case Result.ErrorWithWarnings(warnings, e2) =>
              Result.ErrorWithWarnings(warnings, combineE.combine(e1, e2))
          }

        case Result.ErrorWithWarnings(w1, e1) =>
          second.run(i) match {
            case Result.Success(result) =>
              Result.Success(result)

            case Result.SuccessWithWarnings(warnings, result) =>
              Result.SuccessWithWarnings(warnings, result)

            case Result.Error(e2) =>
              Result.Error(combineE.combine(e1, e2))

            case Result.ErrorWithWarnings(w2, e2) =>
              Result.ErrorWithWarnings(combineW.combine(w1, w2), combineE.combine(e1, e2))
          }
      }

    case c: Validator.ExposeCause[I, e, w, A] =>
      Validator
        .cause(c.underlying)
        .run(i)
        .asInstanceOf[Result[E, W, A]]
  }
}

object Validator {
  final case class Pure[I, E, W, A](underlying: I => Result[E, W, A]) extends Validator[I, E, W, A]

  final case class MapResult[I, E, W, A, B](underlying: Validator[I, E, W, A], f: A => B) extends Validator[I, E, W, B]

  final case class MapWarning[I, E, W1, W2, A](underlying: Validator[I, E, W1, A], f: W1 => W2)
      extends Validator[I, E, W2, A]

  final case class MapError[I, E1, E2, W, A](underlying: Validator[I, E1, W, A], f: E1 => E2)
      extends Validator[I, E2, W, A]

  final case class FlatMap[I, E, W, A, B](
    underlying: Validator[I, E, W, A],
    f: A => Validator[I, E, W, B],
    combineW: Combine[W]
  ) extends Validator[I, E, W, B]

  final case class ZipWith[I, E, W, A, B, C](
    left: Validator[I, E, W, A],
    right: Validator[I, E, W, B],
    f: (A, B) => C,
    combineE: Combine[E],
    combineW: Combine[W]
  ) extends Validator[I, E, W, C]

  final case class Fallback[I, E, W, A](
    first: Validator[I, E, W, A],
    second: Validator[I, E, W, A],
    combineE: Combine[E],
    combineW: Combine[W]
  ) extends Validator[I, E, W, A]

  final case class ExposeCause[I, E, W, A](underlying: Validator[I, E, W, A])
      extends Validator[I, Cause[E], Cause[W], A]

  def cause[I, E, W, A](
    in: Validator[I, E, W, A]
  )(implicit eNotCause: E =:!= Cause[_], wNotCause: W =:!= Cause[_]): Validator[I, Cause[E], Cause[W], A] =
    in match {
      case Pure(underlying)                            => ???
      case MapResult(underlying, f)                    => ???
      case MapWarning(underlying, f)                   => ???
      case MapError(underlying, f)                     => ???
      case FlatMap(underlying, f, combineW)            => ???
      case ZipWith(left, right, f, combineE, combineW) => ???
      case Fallback(first, second, combineE, combineW) => ???
      case ExposeCause(underlying)                     => ???
    }
}
