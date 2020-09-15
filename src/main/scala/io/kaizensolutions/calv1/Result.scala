package io.kaizensolutions.calv1

sealed trait Result[+E, +W, +A] { self =>
  def succeeded: Boolean = self match {
    case Result.Success(_, _) => true
    case Result.Error(_, _)   => false
  }

  def failed: Boolean = !succeeded

  def extractWarnings: Vector[W] = self match {
    case Result.Success(warnings, _) => warnings
    case Result.Error(warnings, _)   => warnings
  }

  def ignoreWarnings: Result[E, Nothing, A] = self match {
    case Result.Success(_, result) => Result.Success(Vector.empty, result)
    case Result.Error(_, error)    => Result.Error(Vector.empty, error)
  }

  def fold[B](errorCase: (Vector[W], E) => B)(successCase: (Vector[W], A) => B): B =
    self match {
      case Result.Success(warnings, result) => successCase(warnings, result)
      case Result.Error(warnings, error)    => errorCase(warnings, error)
    }

  def zipWith[B, C, E1 >: E, W1 >: W](other: Result[E1, W1, B])(f: (A, B) => C): Result[E1, W1, C] =
    (self, other) match {
      case (Result.Success(w1, a), Result.Success(w2, b)) => Result.Success(w1 ++ w2, f(a, b))
      case (Result.Error(w1, e), o)                       => Result.Error(w1 ++ o.extractWarnings, e)
      case (o, Result.Error(w2, e))                       => Result.Error(o.extractWarnings ++ w2, e)
    }

  def zip[B, E1 >: E, W1 >: W](other: Result[E1, W1, B]): Result[E1, W1, (A, B)] = zipWith(other)((_, _))

  def mapResult[B](f: A => B): Result[E, W, B] = self match {
    case s @ Result.Success(_, result) => s.copy(result = f(result))
    case Result.Error(warnings, error) => Result.Error(warnings, error)
  }

  def mapError[EE](f: E => EE): Result[EE, W, A] = self match {
    case Result.Success(warnings, result) => Result.Success(warnings, result)
    case e @ Result.Error(_, error)       => e.copy(error = f(error))
  }

  def mapWarnings[WW](f: W => WW): Result[E, WW, A] = self match {
    case s @ Result.Success(warnings, _) => s.copy(warnings = warnings.map(f))
    case e @ Result.Error(warnings, _)   => e.copy(warnings = warnings.map(f))
  }

  def appendWarning[WW >: W](warning: WW): Result[E, WW, A] = self match {
    case Result.Success(warnings, result) => Result.Success(warnings.appended(warning), result)
    case Result.Error(warnings, error)    => Result.Error(warnings.appended(warning), error)
  }

  def appendWarnings[WW >: W](warnings: Vector[WW]): Result[E, WW, A] = self match {
    case Result.Success(existing, result) => Result.Success(existing ++ warnings, result)
    case Result.Error(existing, error)    => Result.Error(existing ++ warnings, error)
  }

  def prependWarning[WW >: W](warning: WW): Result[E, WW, A] = self match {
    case Result.Success(warnings, result) => Result.Success(warning +: warnings, result)
    case Result.Error(warnings, error)    => Result.Error(warning +: warnings, error)
  }

  def prependWarnings[WW >: W](warnings: Vector[WW]): Result[E, WW, A] = self match {
    case Result.Success(existing, result) => Result.Success(warnings ++ existing, result)
    case Result.Error(existing, error)    => Result.Error(warnings ++ existing, error)
  }

  // if we did not hard-code vector for W, we could define a Then[Error1, Error2]
  // which could express that we tried self but fallback to that
  def fallback[E1 >: E, W1 >: W, A1 >: A](that: Result[E1, W1, A1]): Result[E1, W1, A1] = self match {
    case s @ Result.Success(_, _) => s
    case Result.Error(_, _)       => that
  }
}
object Result {
  final case class Success[+W, +A](warnings: Vector[W], result: A) extends Result[Nothing, W, A]
  final case class Error[+E, +W](warnings: Vector[W], error: E)    extends Result[E, W, Nothing]

  def succeed[A](a: A): Result[Nothing, Nothing, A]                    = Result.Success(Vector.empty, a)
  def succeedAndWarn[A, W](a: A, w: W): Result[Nothing, W, A]          = Result.Success(Vector(w), a)
  def succeedAndWarn[A, W](a: A, ws: Vector[W]): Result[Nothing, W, A] = Result.Success(ws, a)
  def warn[W](w: W): Result[Nothing, W, Unit]                          = Result.Success(Vector(w), ())
  def fail[E](e: E): Result[E, Nothing, Nothing]                       = Result.Error(Vector.empty, e)
}

trait ResultSyntax {
  implicit class ResultOps[A](a: A) {
    def succeed: Result[Nothing, Nothing, A]                    = Result.succeed(a)
    def succeedAndWarn[W](w: W): Result[Nothing, W, A]          = Result.succeedAndWarn(a, w)
    def succeedAndWarn[W](ws: Vector[W]): Result[Nothing, W, A] = Result.succeedAndWarn(a, ws)
    def warn: Result[Nothing, A, Unit]                          = Result.warn(a)
  }
}
