package io.kaizensolutions.nigelv1

import io.kaizensolutions.nigelv1.Result.{Error, Success}

import scala.util.Try

sealed trait Result[+E, +W, +A] {
  def extract: (Vector[W], Either[Vector[E], A])
  def errors: Vector[E]
  def warnings: Vector[W]
  def value: Option[A]

  def map[B](f: A => B): Result[E, W, B]
  def mapWarning[W2](f: W => W2): Result[E, W2, A]
  def mapError[E2](f: E => E2): Result[E2, W, A]

  def flatMap[E2 >: E, W2 >: W, C](f: A => Result[E2, W2, C]): Result[E2, W2, C]

  def ignoreWarnings: Result[E, Nothing, A]

  def zip[E2 >: E, W2 >: W, B](that: Result[E2, W2, B]): Result[E2, W2, (A, B)] =
    (this, that) match {
        case (Error(w1, e1), Error(w2, e2)) => Error(w1 ++ w2, e1 ++ e2)
        case (Error(w1, e1), Success(w2, _)) => Error(w1 ++ w2, e1)
        case (Success(w1, _), Error(w2, e2)) => Error(w1 ++ w2, e2)
        case (Success(w1, r1), Success(w2, r2)) => Success(w1 ++ w2, (r1, r2))
    }
}

object Result {
  final case class Success[W, A](warnings: Vector[W], result: A) extends Result[Nothing, W, A] {
    override def errors: Vector[Nothing] = Vector.empty

    override def value: Option[A] = Some(result)

    override def map[B](f: A => B): Result[Nothing, W, B] = Success(warnings, f(result))

    override def mapWarning[W2](f: W => W2): Result[Nothing, W2, A] = Success(warnings.map(f), result)

    override def mapError[E2](f: Nothing => E2): Result[E2, W, A] = this

    override def flatMap[E2 >: Nothing, W2 >: W, C](f: A => Result[E2, W2, C]): Result[E2, W2, C] = f(result) match {
      case Success(w, r) => Success(warnings ++ w, r)
      case Error(w, r)   => Error(warnings ++ w, r)
    }

    override def ignoreWarnings: Result[Nothing, Nothing, A] = Success(Vector.empty, result)

    override def extract: (Vector[W], Either[Vector[Nothing], A]) = (warnings, Right(result))
  }

  final case class Error[E, W](warnings: Vector[W], result: Vector[E]) extends Result[E, W, Nothing] {
    override def errors: Vector[E] = result

    override def value: Option[Nothing] = None

    override def map[B](f: Nothing => B): Result[E, W, B] = this

    override def mapWarning[W2](f: W => W2): Result[E, W2, Nothing] =
      Error(warnings.map(f), result)

    override def mapError[E2](f: E => E2): Result[E2, W, Nothing] =
      Error(warnings, result.map(f))

    override def ignoreWarnings: Result[E, Nothing, Nothing] = Error(Vector.empty, result)

    override def extract: (Vector[W], Either[Vector[E], Nothing]) = (warnings, Left(errors))

    override def flatMap[E2 >: E, W2 >: W, C](f: Nothing => Result[E2, W2, C]): Result[E2, W2, C] = this
  }

  def success[A](a: A): Result[Nothing, Nothing, A] = Success(Vector.empty, a)
  def warning[W](warning: W): Result[Nothing, W, Unit] =
    Success(Vector(warning), ())

  def error[E](error: E): Result[E, Nothing, Nothing] =
    Error(Vector.empty, Vector(error))

  def fromTry[A](value: Try[A]): Result[Throwable, Nothing, A] =
    value.fold(error, success)
}
