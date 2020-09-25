package io.kaizensolutions.calv4initial

// TODO: Consider laws
// e.g. Cats IO has IO and IO.Par (newtype over IO which is a proper applicative)
sealed trait Result[+E, +W, +A] { self =>
  def succeeded: Boolean = self match {
    case _: Result.Success[W, A] => true
    case _: Result.Error[E, W]   => false
  }

  def failed: Boolean = !succeeded

  def extractWarnings: Option[W] = self match {
    case Result.Success(warning, _) => warning
    case Result.Error(warning, _)   => warning
  }

  def extractError: Option[E] = self match {
    case Result.Success(_, _)   => None
    case Result.Error(_, error) => Option(error)
  }

  def ignoreWarnings: Result[E, Nothing, A] = self match {
    case Result.Success(_, result) => Result.Success(None, result)
    case Result.Error(_, error)    => Result.Error(None, error)
  }

  def fold[B](errorCase: (Option[W], E) => B)(successCase: (Option[W], A) => B): B =
    self match {
      case Result.Success(warn, result) => successCase(warn, result)
      case Result.Error(warn, error)    => errorCase(warn, error)
    }

  def zipWith[B, C, E1 >: E: Combine, W1 >: W: Combine](that: Result[E1, W1, B])(f: (A, B) => C): Result[E1, W1, C] =
    (self, that) match {
      case (Result.Success(w1, a1), Result.Success(w2, a2)) =>
        Result.Success(Combine.combineOption[W1].combine(w1, w2), f(a1, a2))

      case (Result.Success(w1, _), Result.Error(w2, e)) =>
        Result.Error(Combine.combineOption[W1].combine(w1, w2), e)

      case (Result.Error(w1, e), Result.Success(w2, _)) =>
        Result.Error(Combine.combineOption[W1].combine(w1, w2), e)

      case (Result.Error(w1, e1), Result.Error(w2, e2)) =>
        Result.Error(Combine.combineOption[W1].combine(w1, w2), Combine[E1].combine(e1, e2))
    }

  def eitherWith[B, C, E1 >: E: Combine, W1 >: W: Combine](
    that: Result[E1, W1, B]
  )(f: Either[A, B] => C): Result[E1, W1, C] =
    (self, that) match {
      case (Result.Success(w1, a1), Result.Success(w2, _)) =>
        Result.Success(Combine.combineOption[W1].combine(w1, w2), f(Left(a1)))

      case (Result.Success(w1, a1), Result.Error(w2, _)) =>
        Result.Success(Combine.combineOption[W1].combine(w1, w2), f(Left(a1)))

      case (Result.Error(w1, e), Result.Success(w2, a2)) =>
        Result.Success(Combine.combineOption[W1].combine(w1, w2), f(Right(a2)))

      case (Result.Error(w1, e1), Result.Error(w2, e2)) =>
        Result.Error(Combine.combineOption[W1].combine(w1, w2), Combine[E1].combine(e1, e2))
    }

  def map[B](f: A => B): Result[E, W, B] = self match {
    case Result.Success(warning, result) => Result.Success(warning, f(result))
    case Result.Error(warning, error)    => Result.Error(warning, error)
  }

  def mapError[E1](f: E => E1): Result[E1, W, A] = self match {
    case Result.Success(warning, result) => Result.Success(warning, result)
    case Result.Error(warning, error)    => Result.Error(warning, f(error))
  }

  def mapWarning[W1](f: W => W1): Result[E, W1, A] = self match {
    case Result.Success(warning, result) => Result.Success(warning.map(f), result)
    case Result.Error(warning, error)    => Result.Error(warning.map(f), error)
  }

  def flatMap[E1 >: E, W1 >: W: Combine, B](
    f: A => Result[E1, W1, B]
  ): Result[E1, W1, B] =
    self match {
      case Result.Success(w1, a) =>
        f(a) match {
          case Result.Success(w2, b) =>
            Result.Success(Combine.combineOption[W1].combine(w1, w2), b)

          case Result.Error(w2, error) =>
            Result.Error(Combine.combineOption[W1].combine(w1, w2), error)

        }

      case Result.Error(warning, error) =>
        Result.Error(warning, error)
    }
}

object Result {
  final case class Success[+W, +A](warning: Option[W], result: A) extends Result[Nothing, W, A]
  final case class Error[+W, +E](warning: Option[W], error: E)    extends Result[E, W, Nothing]

  def succeed[A](a: A): Result[Nothing, Nothing, A]        = Result.Success(None, a)
  def succeedWarn[W, A](w: W, a: A): Result[Nothing, W, A] = Result.Success(Option(w), a)
  def fail[E](e: E): Result[E, Nothing, Nothing]           = Result.Error(None, e)
  def failWarn[E, W](w: W, e: E): Result[E, W, Nothing]    = Result.Error(Option(w), e)
}

trait ResultSyntax {
  implicit class ResultOps[A](a: A) {
    def fail[E](e: E): Result[E, Nothing, Nothing]           = Result.fail(e)
    def failAndWarn[E, W](e: E, w: W): Result[E, W, Nothing] = Result.failWarn(w, e)
    def succeed: Result[Nothing, Nothing, A]                 = Result.succeed(a)
    def succeedAndWarn[W](w: W): Result[Nothing, W, A]       = Result.succeedWarn(w, a)
  }
}
