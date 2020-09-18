package io.kaizensolutions.calvin3initial

import shapeless.=:!=

sealed trait Validator[-I, +E, +W, +A] { self =>
  def zipWith[I1 <: I, E1 >: E, W1 >: W: Combine, B, C](that: Validator[I1, E1, W1, B])(
    f: (A, B) => C
  ): Validator[I1, E1, W1, C] = Validator.ZipWith(self, that, f, Combine.left, Combine[W1])

  def zipRight[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, B] =
    zipWith(that)((_, b) => b)

  def zipLeft[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, A] =
    zipWith(that)((a, _) => a)

  def and[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, (A, B)] =
    zipWith(that)((_, _))

  def or[I1 <: I, E1 >: E, W1 >: W, A1 >: A](that: Validator[I1, E1, W1, A1]): Validator[I1, E1, W1, A1] =
    Validator.Fallback(self, that, Combine.right, Combine.right)

  def fallback[I1 <: I, E1 >: E, W1 >: W, A1 >: A](that: Validator[I1, E1, W1, A1]): Validator[I1, E1, W1, A1] =
    or(that)

  def andThen[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[A, E1, W1, B]): Validator[I1, E1, W1, B] =
    Validator.AndThen(self, that, Combine.right, Combine[W1])

  def flatMap[I1 <: I, E1 >: E, W1 >: W: Combine, B](f: A => Validator[I1, E1, W1, B]): Validator[I1, E1, W1, B] =
    Validator.FlatMap(self, f, Combine[W1])

  def map[B](f: A => B): Validator[I, E, W, B] = Validator.MapResult(self, f)

  def mapResult[B](f: A => B): Validator[I, E, W, B] = map(f)

  def asResult[B](b: B): Validator[I, E, W, B] = mapResult(_ => b)

  def mapError[E1](f: E => E1): Validator[I, E1, W, A] = Validator.MapError(self, f)

  def mapErrorWithInput[I1 <: I, E1](f: (I1, E) => E1): Validator[I1, E1, W, A] = Validator.MapErrorWithInput(self, f)

  def asError[E1](e: E1): Validator[I, E1, W, A] = mapError(_ => e)

  def asErrorWithInput[I1 <: I, E1](f: I1 => E1): Validator[I1, E1, W, A] = mapErrorWithInput((i, _) => f(i))

  def mapWarning[WW](f: W => WW): Validator[I, E, WW, A] = Validator.MapWarning(self, f)

  def asWarning[W1](w: W1): Validator[I, E, W1, A] = Validator.WithWarning(self, w)

  def contramapInput[II](f: II => I): Validator[II, E, W, A] = Validator.MapInput(self, f)

  def exposeCause[I1 <: I, W1 >: W, E1 >: E, A1 >: A](implicit
    eNotCause: E1 =:!= Cause[_],
    wNotCause: W1 =:!= Cause[_]
  ): Validator[I, Cause[E], Cause[W], A] = Validator.ExposeCause(self)

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

    case Validator.WithWarning(underlying, w) =>
      underlying.run(i) match {
        case Result.Success(result)                       => Result.SuccessWithWarnings(w, result)
        case Result.SuccessWithWarnings(warnings, result) => Result.SuccessWithWarnings(w, result)
        case Result.Error(error)                          => Result.ErrorWithWarnings(w, error)
        case Result.ErrorWithWarnings(warnings, error)    => Result.ErrorWithWarnings(w, error)
      }

    case Validator.MapInput(underlying, f) =>
      underlying.run(f(i))

    case Validator.MapResult(underlying, f) =>
      underlying.run(i).mapResult(f)

    case Validator.MapError(underlying, f) =>
      underlying.run(i).mapError(f)

    case Validator.MapErrorWithInput(underlying, f) =>
      underlying.run(i).mapError(e => f(i, e))

    case Validator.MapWarning(underlying, f) =>
      underlying.run(i).mapWarning(f)

    case Validator.AndThen(first, second, combineE, combineW) =>
      val a = first.run(i)
      a match {
        case Result.Success(result) =>
          second.run(result)

        case sw @ Result.SuccessWithWarnings(_, result) =>
          sw.zipWith(second.run(result))((_, r) => r)(combineE, combineW)

        case e @ Result.Error(error) =>
          e

        case ew @ Result.ErrorWithWarnings(_, _) =>
          ew
      }

    case Validator.FlatMap(underlying, f, combineW) =>
      underlying
        .run(i)
        .flatMap(result => f(result).run(i))(combineW)

    case zipCase: Validator.ZipWith[I, E, W, A, b, c] =>
      val left  = zipCase.left.run(i)
      val right = zipCase.right.run(i)
      val f     = zipCase.f
      val cW    = zipCase.combineW
      val cE    = zipCase.combineE
      left.zipWith(right)(f)(cE, cW)

    case Validator.Fallback(first, second, combineE, combineW) =>
      first.run(i).fallback(second.run(i))(combineE, combineW)

    case c: Validator.ExposeCause[I, e, w, A] =>
      Validator
        .peelCause(c.underlying)
        .run(i)
        .asInstanceOf[Result[E, W, A]]
  }
}

object Validator {
  final case class Pure[I, E, W, A](underlying: I => Result[E, W, A]) extends Validator[I, E, W, A]

  final case class WithWarning[I, E, W, A](underlying: Validator[I, E, Any, A], w: W) extends Validator[I, E, W, A]

  final case class MapInput[I1, I2, E, W, A](underlying: Validator[I2, E, W, A], f: I1 => I2)
      extends Validator[I1, E, W, A]

  final case class MapResult[I, E, W, A, B](underlying: Validator[I, E, W, A], f: A => B) extends Validator[I, E, W, B]

  final case class MapErrorWithInput[I, E1, E2, W, A](underlying: Validator[I, E1, W, A], f: (I, E1) => E2)
      extends Validator[I, E2, W, A]

  final case class MapWarning[I, E, W1, W2, A](underlying: Validator[I, E, W1, A], f: W1 => W2)
      extends Validator[I, E, W2, A]

  final case class MapError[I, E1, E2, W, A](underlying: Validator[I, E1, W, A], f: E1 => E2)
      extends Validator[I, E2, W, A]

  final case class AndThen[I, E, W, A, B](
    first: Validator[I, E, W, A],
    second: Validator[A, E, W, B],
    combineE: Combine[E],
    combineW: Combine[W]
  ) extends Validator[I, E, W, B]

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

  /**
   * Rewrites the execution tree to examine exactly how we got the result
   * @param in
   * @tparam I
   * @tparam E
   * @tparam W
   * @tparam A
   * @return
   */
  def peelCause[I, E, W, A](
    in: Validator[I, E, W, A]
  ): Validator[I, Cause[E], Cause[W], A] =
    in match {
      case p @ Pure(_) =>
        p.mapWarning(Cause.single).mapError(Cause.single)

      case ww: WithWarning[I, e, w, a] =>
        peelCause(ww.underlying.asWarning(ww.w))

      case MapInput(underlying, f) =>
        peelCause(underlying).contramapInput(f)

      case MapResult(underlying, f) =>
        peelCause(underlying).mapResult(f)

      case mw: MapWarning[I, E, w1, w2, A] =>
        peelCause(mw.underlying).mapWarning(_.map(mw.f))

      case me: MapError[I, e1, e2, W, A] =>
        peelCause(me.underlying).mapError(_.map(me.f))

      case me: MapErrorWithInput[I, e1, e2, W, A] =>
        MapErrorWithInput[I, Cause[e1], Cause[e2], Cause[W], A](
          peelCause(me.underlying),
          (i, causeE1) => causeE1.map(e1 => me.f(i, e1))
        )

      case at: AndThen[I, e, w, a, b] =>
        AndThen[I, Cause[e], Cause[w], a, b](
          peelCause(at.first),
          peelCause(at.second),
          Cause.causeThenCombine,
          Cause.causeThenCombine
        )

      case fm: FlatMap[I, e, w, a, b] =>
        FlatMap[I, Cause[e], Cause[w], a, b](
          peelCause(fm.underlying),
          a => peelCause(fm.f(a)),
          Cause.causeThenCombine
        )

      case zw: ZipWith[I, e, w, a, b, c] =>
        ZipWith[I, Cause[e], Cause[w], a, b, c](
          peelCause(zw.left),
          peelCause(zw.right),
          zw.f,
          Cause.causeBothCombine,
          Cause.causeBothCombine
        )

      case fb: Fallback[I, e, w, a] =>
        Fallback[I, Cause[e], Cause[w], a](
          peelCause(fb.first),
          peelCause(fb.second),
          Cause.causeFallbackCombine,
          Cause.causeFallbackCombine
        )

      case ec: ExposeCause[I, e, w, a] =>
        peelCause(ec.underlying).asInstanceOf[Validator[I, Cause[E], Cause[W], A]]
    }
}
