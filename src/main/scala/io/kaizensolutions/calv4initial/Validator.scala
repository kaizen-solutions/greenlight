package io.kaizensolutions.calv4initial

sealed trait Validator[-I, +E, +W, +A] { self =>
  def zipWith[I1 <: I, E1 >: E, W1 >: W: Combine, B, C](that: Validator[I1, E1, W1, B])(
    f: (A, B) => C
  ): Validator[I1, E1, W1, C] = Validator.ZipWith(self, that, f, Combine.left[E1], Combine[W1])

  def zipRight[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, B] =
    zipWith(that)((_, b) => b)

  def zipLeft[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, A] =
    zipWith(that)((a, _) => a)

  def and[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[I1, E1, W1, B]): Validator[I1, E1, W1, (A, B)] =
    zipWith(that)((_, _))

  def eitherWith[I1 <: I, E1 >: E: Combine, W1 >: W: Combine, B, C](that: Validator[I1, E1, W1, B])(
    f: Either[A, B] => C
  ): Validator[I1, E1, W1, C] = Validator.EitherWith(self, that, f, Combine[E1], Combine[W1])

  def or[I1 <: I, E1 >: E, W1 >: W, A1 >: A](that: Validator[I1, E1, W1, A1]): Validator[I1, E1, W1, A1] =
    eitherWith(that) {
      case Left(a)  => a
      case Right(a) => a
    }(Combine.left[E1], Combine.left[W1])

  def fallback[I1 <: I, E1 >: E, W1 >: W, A1 >: A](that: Validator[I1, E1, W1, A1]): Validator[I1, E1, W1, A1] =
    or(that)

  def andThen[I1 <: I, E1 >: E, W1 >: W: Combine, B](that: Validator[A, E1, W1, B]): Validator[I1, E1, W1, B] =
    Validator.AndThen(self, that, Combine.right[E1], Combine[W1])

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

  def asWarning[WW](w: WW): Validator[I, E, WW, A] = Validator.MapWarning(self, (_: W) => w)

  def contramapInput[II](f: II => I): Validator[II, E, W, A] = Validator.MapInput(self, f)

  def run(i: I): Result[E, W, A] = self match {
    case Validator.Pure(underlying) =>
      underlying(i)

    case Validator.MapInput(underlying, f) =>
      underlying.run(f(i))

    case Validator.MapResult(underlying, f) =>
      underlying.run(i).map(f)

    case Validator.MapError(underlying, f) =>
      underlying.run(i).mapError(f)

    case Validator.MapErrorWithInput(underlying, f) =>
      underlying.run(i).mapError(e => f(i, e))

    case Validator.MapWarning(underlying, f) =>
      underlying.run(i).mapWarning(f)

    case Validator.AndThen(first, second, combineE, combineW) =>
      first.run(i).flatMap(second.run)(combineW)

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

    case eitherCase: Validator.EitherWith[I, E, W, A, b, c] =>
      val left: Result[E, W, A]  = eitherCase.first.run(i)
      val right: Result[E, W, b] = eitherCase.second.run(i)
      val f: Either[A, b] => c   = eitherCase.f
      val cW: Combine[W]         = eitherCase.combineW
      val cE: Combine[E]         = eitherCase.combineE
      left.eitherWith(right)(f)(cE, cW)
  }
}

object Validator {
  final case class Pure[I, E, W, A](underlying: I => Result[E, W, A]) extends Validator[I, E, W, A]

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

  final case class EitherWith[I, E, W, A, B, C](
    first: Validator[I, E, W, A],
    second: Validator[I, E, W, B],
    f: Either[A, B] => C,
    combineE: Combine[E],
    combineW: Combine[W]
  ) extends Validator[I, E, W, C]
}
