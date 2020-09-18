package io.kaizensolutions.calvin3initial

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

  def zipWith[B, C, E1 >: E: Combine, W1 >: W: Combine](
    other: Result[E1, W1, B]
  )(f: (A, B) => C): Result[E1, W1, C] = {
    val fW = Combine[W1].combine _
    val fE = Combine[E1].combine _

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
  }

  def zip[B, E1 >: E: Combine, W1 >: W: Combine](other: Result[E1, W1, B]): Result[E1, W1, (A, B)] =
    zipWith(other)((_, _))

  def zipRight[B, E1 >: E: Combine, W1 >: W: Combine](
    other: Result[E1, W1, B]
  ): Result[E1, W1, B] =
    zipWith(other)((_, r) => r)

  def zipLeft[B, E1 >: E: Combine, W1 >: W: Combine](
    other: Result[E1, W1, B]
  ): Result[E1, W1, A] =
    zipWith(other)((l, _) => l)

  def <*>[B, E1 >: E: Combine, W1 >: W: Combine](
    other: Result[E1, W1, B]
  ): Result[E1, W1, (A, B)] =
    zip(other)

  def *>[B, E1 >: E: Combine, W1 >: W: Combine](other: Result[E1, W1, B]): Result[E1, W1, B] =
    zipRight(other)

  def <*[B, E1 >: E: Combine, W1 >: W: Combine](other: Result[E1, W1, B]): Result[E1, W1, A] =
    zipLeft(other)

  def flatMap[E1 >: E, W1 >: W: Combine, B](f: A => Result[E1, W1, B]): Result[E1, W1, B] =
    self match {
      case Result.Success(result) =>
        f(result)

      case Result.SuccessWithWarnings(warnings, result) =>
        f(result) match {
          case Result.Success(result) =>
            Result.SuccessWithWarnings(warnings, result)

          case Result.SuccessWithWarnings(resultWarnings, result) =>
            Result.SuccessWithWarnings(Combine[W1].combine(warnings, resultWarnings), result)

          case Result.Error(error) =>
            Result.ErrorWithWarnings(warnings, error)

          case Result.ErrorWithWarnings(resultWarnings, error) =>
            Result.ErrorWithWarnings(Combine[W1].combine(warnings, resultWarnings), error)
        }

      case Result.Error(error) =>
        Result.Error(error)

      case Result.ErrorWithWarnings(warnings, error) =>
        Result.ErrorWithWarnings(warnings, error)
    }

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

  def fallback[E1 >: E: Combine, W1 >: W: Combine, A1 >: A](that: => Result[E1, W1, A1]): Result[E1, W1, A1] =
    self match {
      case Result.Success(result) =>
        Result.Success(result)

      case Result.SuccessWithWarnings(w1, result) =>
        Result.SuccessWithWarnings(w1, result)

      case Result.Error(e1) =>
        that match {
          case Result.Success(result) =>
            Result.Success(result)

          case Result.SuccessWithWarnings(warnings, result) =>
            Result.SuccessWithWarnings(warnings, result)

          case Result.Error(e2) =>
            Result.Error(Combine[E1].combine(e1, e2))

          case Result.ErrorWithWarnings(warnings, e2) =>
            Result.ErrorWithWarnings(warnings, Combine[E1].combine(e1, e2))
        }

      case Result.ErrorWithWarnings(w1, e1) =>
        that match {
          case Result.Success(result) =>
            Result.Success(result)

          case Result.SuccessWithWarnings(warnings, result) =>
            Result.SuccessWithWarnings(warnings, result)

          case Result.Error(e2) =>
            Result.Error(Combine[E1].combine(e1, e2))

          case Result.ErrorWithWarnings(w2, e2) =>
            Result.ErrorWithWarnings(Combine[W1].combine(w1, w2), Combine[E1].combine(e1, e2))
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
