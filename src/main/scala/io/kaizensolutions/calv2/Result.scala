package io.kaizensolutions.calv2

trait Combine[A] {
  def combine(a1: A, a2: A): A
}
object Combine extends LowPriorityCombine {}
trait LowPriorityCombine {
  implicit def combineSeq[A]: Combine[Seq[A]] =
    (a1: Seq[A], a2: Seq[A]) => a1 ++ a2
}

// Idea similar to https://github.com/zio/zio-prelude/pull/229
// see if their encoding is better
sealed trait Cause[+A] extends Serializable with Product
object Cause {
  case class Single[A](a: A)                                extends Cause[A]
  case class Both[A](a: Cause[A], b: Cause[A])              extends Cause[A]
  case class Then[A](first: Cause[A], second: Cause[A])     extends Cause[A]
  case class Fallback[A](first: Cause[A], second: Cause[A]) extends Cause[A]
  case object Empty                                         extends Cause[Nothing]

  def empty[A]: Cause[A]                              = Empty
  def single[A](a: A): Cause[A]                       = Cause.Single(a)
  def both[A](a: Cause[A], b: Cause[A]): Cause[A]     = Cause.Both(a, b)
  def andThen[A](a: Cause[A], b: Cause[A]): Cause[A]  = Cause.Then(a, b)
  def fallback[A](a: Cause[A], b: Cause[A]): Cause[A] = Cause.Fallback(a, b)

  def causeBothCombine[A]: Combine[Cause[A]]     = (a1: Cause[A], a2: Cause[A]) => Cause.both(a1, a2)
  def causeThenCombine[A]: Combine[Cause[A]]     = (a1: Cause[A], a2: Cause[A]) => Cause.andThen(a1, a2)
  def causeFallbackCombine[A]: Combine[Cause[A]] = (a1: Cause[A], a2: Cause[A]) => Cause.fallback(a1, a2)
}

sealed trait Result[+E, +W, +A] { self =>

  def succeeded: Boolean = self match {
    case Result.Success(_) | Result.SuccessWithWarnings(_, _) => true
    case Result.Error(_) | Result.ErrorWithWarnings(_, _)     => false
  }

  def failed: Boolean = !succeeded

  def extractWarnings: Option[W] = self match {
    case Result.Success(_) | Result.Error(_)     => None
    case Result.ErrorWithWarnings(warnings, _)   => Option(warnings)
    case Result.SuccessWithWarnings(warnings, _) => Option(warnings)
  }

  def extractError: Option[E] = self match {
    case Result.Success(_)                  => None
    case Result.SuccessWithWarnings(_, _)   => None
    case Result.Error(error)                => Option(error)
    case Result.ErrorWithWarnings(_, error) => Option(error)
  }

  def ignoreWarnings: Result[E, Nothing, A] = self match {
    case s @ Result.Success(_)                 => s
    case e @ Result.Error(_)                   => e
    case Result.ErrorWithWarnings(_, error)    => Result.Error(error)
    case Result.SuccessWithWarnings(_, result) => Result.Success(result)
  }

  def fold[B](errorCase: (Option[W], E) => B)(successCase: (Option[W], A) => B): B =
    self match {
      case Result.Success(result)                       => successCase(None, result)
      case Result.SuccessWithWarnings(warnings, result) => successCase(Option(warnings), result)
      case Result.Error(error)                          => errorCase(None, error)
      case Result.ErrorWithWarnings(warnings, error)    => errorCase(Option(warnings), error)
    }

  def zipWith[B, C, E1 >: E, W1 >: W](
    other: Result[E1, W1, B]
  )(f: (A, B) => C)(implicit C: Combine[W1]): Result[E1, W1, C] =
    (self, other) match {
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
        Result.SuccessWithWarnings(C.combine(w1, w2), f(a, b))

      case (Result.SuccessWithWarnings(w1, a), Result.Error(e)) =>
        Result.ErrorWithWarnings(w1, e)

      case (Result.SuccessWithWarnings(w1, a), Result.ErrorWithWarnings(w2, e)) =>
        Result.ErrorWithWarnings(C.combine(w1, w2), e)

      case (Result.ErrorWithWarnings(w1, e1), Result.Success(b)) =>
        Result.ErrorWithWarnings(w1, e1)

      case (Result.ErrorWithWarnings(w1, e1), Result.Error(b)) =>
        Result.ErrorWithWarnings(w1, e1)

      case (Result.ErrorWithWarnings(w1, e1), Result.SuccessWithWarnings(w2, b)) =>
        Result.ErrorWithWarnings(C.combine(w1, w2), e1)

      case (Result.ErrorWithWarnings(w1, e1), Result.ErrorWithWarnings(w2, e2)) =>
        Result.ErrorWithWarnings(C.combine(w1, w2), e1)

      case (Result.Error(e1), Result.Error(b)) =>
        Result.Error(e1)

      case (Result.Error(e1), Result.Success(b)) =>
        Result.Error(e1)

      case (Result.Error(e1), Result.SuccessWithWarnings(w2, b)) =>
        Result.ErrorWithWarnings(w2, e1)

      case (Result.Error(e1), Result.ErrorWithWarnings(w2, e2)) =>
        Result.ErrorWithWarnings(w2, e1)
    }

  def zipWithCause[B, C, E1 >: E, W1 >: W](other: Result[E1, W1, B])(f: (A, B) => C): Result[Cause[E1], Cause[W1], C] =
    (self, other) match {
      case (Result.Success(a), Result.Success(b)) =>
        Result.Success(f(a, b))

      case (Result.Success(a), Result.SuccessWithWarnings(w, b)) =>
        Result.SuccessWithWarnings(Cause.single(w), f(a, b))

      case (Result.Success(a), Result.Error(b)) =>
        Result.Error(Cause.single(b))

      case (Result.Success(a), Result.ErrorWithWarnings(w, b)) =>
        Result.ErrorWithWarnings(Cause.single(w), Cause.single(b))

      case (Result.SuccessWithWarnings(w1, a), Result.Success(b)) =>
        Result.SuccessWithWarnings(Cause.single(w1), f(a, b))

      case (Result.SuccessWithWarnings(w1, a), Result.SuccessWithWarnings(w2, b)) =>
        Result.SuccessWithWarnings(Cause.both(Cause.single(w1), Cause.single(w2)), f(a, b))

      case (Result.SuccessWithWarnings(w1, a), Result.Error(e)) =>
        Result.ErrorWithWarnings(Cause.single(w1), Cause.single(e))

      case (Result.SuccessWithWarnings(w1, a), Result.ErrorWithWarnings(w2, e)) =>
        Result.ErrorWithWarnings(Cause.both(Cause.single(w1), Cause.single(w2)), Cause.single(e))

      case (Result.ErrorWithWarnings(w1, e1), Result.Success(b)) =>
        Result.ErrorWithWarnings(Cause.single(w1), Cause.single(e1))

      case (Result.ErrorWithWarnings(w1, e1), Result.Error(b)) =>
        Result.ErrorWithWarnings(Cause.single(w1), Cause.single(e1))

      case (Result.ErrorWithWarnings(w1, e1), Result.SuccessWithWarnings(w2, b)) =>
        Result.ErrorWithWarnings(Cause.both(Cause.single(w1), Cause.single(w2)), Cause.single(e1))

      case (Result.ErrorWithWarnings(w1, e1), Result.ErrorWithWarnings(w2, e2)) =>
        Result.ErrorWithWarnings(
          Cause.both(Cause.single(w1), Cause.single(w2)),
          Cause.both(Cause.single(e1), Cause.single(e2))
        )

      case (Result.Error(e1), Result.Error(e2)) =>
        Result.Error(Cause.both(Cause.single(e1), Cause.single(e2)))

      case (Result.Error(e1), Result.Success(b)) =>
        Result.Error(Cause.single(e1))

      case (Result.Error(e1), Result.SuccessWithWarnings(w2, b)) =>
        Result.ErrorWithWarnings(Cause.single(w2), Cause.single(e1))

      case (Result.Error(e1), Result.ErrorWithWarnings(w2, e2)) =>
        Result.ErrorWithWarnings(Cause.single(w2), Cause.single(e1))
    }

  def zip[B, E1 >: E, W1 >: W](other: Result[E1, W1, B])(implicit C: Combine[W1]): Result[E1, W1, (A, B)] =
    zipWith(other)((_, _))

  def zipRight[B, E1 >: E, W1 >: W](other: Result[E1, W1, B])(implicit C: Combine[W1]): Result[E1, W1, B] =
    zipWith(other)((_, r) => r)

  def zipLeft[B, E1 >: E, W1 >: W](other: Result[E1, W1, B])(implicit C: Combine[W1]): Result[E1, W1, A] =
    zipWith(other)((l, _) => l)

  def <*>[B, E1 >: E, W1 >: W](other: Result[E1, W1, B])(implicit C: Combine[W1]): Result[E1, W1, (A, B)] =
    zip(other)

  def *>[B, E1 >: E, W1 >: W](other: Result[E1, W1, B])(implicit C: Combine[W1]): Result[E1, W1, B] =
    zipRight(other)

  def <*[B, E1 >: E, W1 >: W](other: Result[E1, W1, B])(implicit C: Combine[W1]): Result[E1, W1, A] =
    zipLeft(other)

  def mapResult[B](f: A => B): Result[E, W, B] = self match {
    case s @ Result.Success(result)                => s.copy(result = f(result))
    case s @ Result.SuccessWithWarnings(_, result) => s.copy(result = f(result))
    case e @ Result.Error(_)                       => e
    case e @ Result.ErrorWithWarnings(_, _)        => e
  }

  def mapError[EE](f: E => EE): Result[EE, W, A] = self match {
    case s @ Result.Success(_)                  => s
    case s @ Result.SuccessWithWarnings(_, _)   => s
    case e @ Result.Error(error)                => e.copy(error = f(error))
    case e @ Result.ErrorWithWarnings(_, error) => e.copy(error = f(error))
  }

  def mapWarning[WW](f: W => WW): Result[E, WW, A] = self match {
    case s @ Result.Success(_)                       => s
    case e @ Result.Error(_)                         => e
    case s @ Result.SuccessWithWarnings(warnings, _) => s.copy(warnings = f(warnings))
    case e @ Result.ErrorWithWarnings(warnings, _)   => e.copy(warnings = f(warnings))
  }

  def appendWarning[WW >: W](warning: WW)(implicit C: Combine[WW]): Result[E, WW, A] = self match {
    case Result.Success(result) =>
      Result.SuccessWithWarnings(warning, result)

    case Result.SuccessWithWarnings(existing, result) =>
      Result.SuccessWithWarnings(C.combine(existing, warning), result)

    case Result.ErrorWithWarnings(existing, error) =>
      Result.ErrorWithWarnings(C.combine(existing, warning), error)

    case Result.Error(error) =>
      Result.ErrorWithWarnings(warning, error)
  }

  def prependWarning[WW >: W](warning: WW)(implicit C: Combine[WW]): Result[E, WW, A] = self match {
    case Result.Success(result) =>
      Result.SuccessWithWarnings(warning, result)

    case Result.SuccessWithWarnings(existing, result) =>
      Result.SuccessWithWarnings(C.combine(warning, existing), result)

    case Result.Error(error) =>
      Result.ErrorWithWarnings(warning, error)

    case Result.ErrorWithWarnings(existing, error) =>
      Result.ErrorWithWarnings(C.combine(warning, existing), error)
  }

  def fallback[E1 >: E, W1 >: W, A1 >: A](that: Result[E1, W1, A1]): Result[E1, W1, A1] = self match {
    case s @ Result.Success(_)                            => s
    case s @ Result.SuccessWithWarnings(_, _)             => s
    case Result.Error(_) | Result.ErrorWithWarnings(_, _) => that
  }

  def fallbackCause[E1 >: E, W1 >: W, A1 >: A](that: Result[E1, W1, A1]): Result[Cause[E1], Cause[W1], A1] =
    self match {
      case s @ Result.Success(_) => s
      case Result.SuccessWithWarnings(warnings, result) =>
        Result.SuccessWithWarnings(Cause.single(warnings), result)

      case e @ Result.Error(_) =>
        that match {
          case Result.Success(_) =>
            e.mapError(Cause.single)

          case Result.SuccessWithWarnings(warnings, _) =>
            e.mapError(Cause.single)
              .appendWarning(Cause.single(warnings))(Cause.causeThenCombine)

          case Result.Error(e2) =>
            e.mapError(e1 => Cause.andThen(Cause.single(e1), Cause.single(e2)))

          case Result.ErrorWithWarnings(w2, e2) =>
            e.mapError(e1 => Cause.andThen(Cause.single(e1), Cause.single(e2)))
              .appendWarning(Cause.single(w2))(Cause.causeThenCombine)
        }

      case e @ Result.ErrorWithWarnings(_, _) =>
        that match {
          case Result.Success(_) =>
            e.mapError(Cause.single)
              .mapWarning(Cause.single)

          case Result.SuccessWithWarnings(w2, _) =>
            e.mapError(Cause.single)
              .mapWarning(w1 => Cause.andThen(Cause.single(w1), Cause.single(w2)))

          case Result.Error(e2) =>
            e.mapError(e1 => Cause.andThen(Cause.single(e1), Cause.single(e2)))
              .mapWarning(Cause.single)

          case Result.ErrorWithWarnings(w2, e2) =>
            e.mapError(e1 => Cause.andThen(Cause.single(e1), Cause.single(e2)))
              .mapWarning(w1 => Cause.andThen(Cause.single(w1), Cause.single(w2)))
        }
    }
}
object Result {
  final case class Success[+A](result: A)                              extends Result[Nothing, Nothing, A]
  final case class SuccessWithWarnings[+W, +A](warnings: W, result: A) extends Result[Nothing, W, A]
  final case class Error[+E](error: E)                                 extends Result[E, Nothing, Nothing]
  final case class ErrorWithWarnings[+E, +W](warnings: W, error: E)    extends Result[E, W, Nothing]

  def succeed[A](a: A): Result[Nothing, Nothing, A]           = Result.Success(a)
  def succeedAndWarn[A, W](a: A, w: W): Result[Nothing, W, A] = Result.SuccessWithWarnings(w, a)
  def warn[W](w: W): Result[Nothing, W, Unit]                 = Result.SuccessWithWarnings(w, ())
  def fail[E](e: E): Result[E, Nothing, Nothing]              = Result.Error(e)
  def failAndWarn[E, W](e: E, w: W): Result[E, W, Nothing]    = Result.ErrorWithWarnings(w, e)
}

trait ResultSyntax {
  implicit class ResultOps[A](a: A) {
    def fail[E](e: E): Result[E, Nothing, Nothing]           = Result.fail(e)
    def failAndWarn[E, W](e: E, w: W): Result[E, W, Nothing] = Result.failAndWarn(e, w)
    def warn: Result[Nothing, A, Unit]                       = Result.warn(a)
    def succeed: Result[Nothing, Nothing, A]                 = Result.succeed(a)
    def succeedAndWarn[W](w: W): Result[Nothing, W, A]       = Result.succeedAndWarn(a, w)
  }
}
