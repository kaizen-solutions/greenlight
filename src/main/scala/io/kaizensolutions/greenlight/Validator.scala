package io.kaizensolutions.greenlight

import io.kaizensolutions.greenlight.Validator._
import shapeless.Lub

import scala.util.Try

sealed trait Validator[-I, +E, +W, +A] extends Product with Serializable {
  def errors: Vector[E]
  def warnings: Vector[W]
  def value: Option[A]
  def run(i: I): Result[E, W, A]
  def map[B](f: A => B): Validator[I, E, W, B]
  def mapWarning[W2](f: W => W2): Validator[I, E, W2, A]
  def mapError[E2](f: E => E2): Validator[I, E2, W, A]
  def contramap[I2](f: I2 => I): Validator[I2, E, W, A]
  def flatMap[I2 <: I, E2 >: E, W2 >: W, C](f: A => Validator[I2, E2, W2, C]): Validator[I2, E2, W2, C]
  def ignoreWarnings: Validator[I, E, Nothing, A]

  def andThen[E2 >: E, W2 >: W, B](that: Validator[A, E2, W2, B]): Validator[I, E2, W2, B] =
    convert(i => this.run(i).flatMap(that.run).run(null))

  def zip[I2 <: I, E2 >: E, W2 >: W, B](that: Validator[I2, E2, W2, B]): Validator[I2, E2, W2, (A, B)] =
    convert { i =>
      val aVal = this.run(i)
      val bVal = that.run(i)

      (aVal, bVal) match {
        case (Error(w1, e1), Error(w2, e2)) => Error(w1 ++ w2, e1 ++ e2)
        case (Error(w1, e1), Success(w2, _)) => Error(w1 ++ w2, e1)
        case (Success(w1, _), Error(w2, e2)) => Error(w1 ++ w2, e2)
        case (Success(w1, r1), Success(w2, r2)) => Success(w1 ++ w2, (r1, r2))
      }
    }

  def focusOn[B](f: A => B): Validator[I, E, W, B] =
    map(f)

  def and[I2, E2 >: E, W2 >: W, B](that: Validator[I2, E2, W2, B]): Validator[(I, I2), E2, W2, (A, B)] =
    convert { (tuple: (I, I2)) =>
      val aVal = this.run(tuple._1)
      val bVal = that.run(tuple._2)

      (aVal zip bVal).run(null)
    }

  def or[I2 <: I, E2, W2 >: W, B >: A, EW2](that: Validator[I2, E2, W2, B])(implicit lub: Lub[E, W2, EW2]): Validator[I2, E2, EW2, B] =
    convert { i =>
      this.run(i) match {
        case Success(warnings, result) => Success(warnings.map(lub.right), result)
        case Error(w1, e1) =>
          that.run(i) match {
            case Success(w2, r) => Success((w1 ++ w2).map(lub.right) ++ e1.map(lub.left), r)
            case Error(w2, e2) => Error((w1 ++ w2).map(lub.right) ++ e1.map(lub.left), e2)
          }
      }
    }

  def as[A2](value: A2): Validator[I, E, W, A2] =
    this.map(_ => value)

  def withWarning[I2 <: I, W2](handler: I2 => W2): Validator[I2, E, W2, A] =
    convert(i => this.run(i) match {
      case Error(_, errors) => Error(Vector(handler(i)), errors)
      case Success(_, result) => Success(Vector(handler(i)), result)
    })

  def withError[I2 <: I, E2](handler: I2 => E2): Validator[I2, E2, W, A] =
    convert(i => this.run(i) match {
      case Error(warnings, _) => Error(warnings, Vector(handler(i)))
      case Success(warnings, result) => Success(warnings, result)
    })

  def asWarning[W2](warning: W2): Validator[I, E, W2, A] = withWarning(_ => warning)
  def asError[E2](error: E2): Validator[I, E2, W, A] = withError(_ => error)
}

sealed trait Result[+E, +W, +A] extends Validator[Any, E, W, A] {
  def extract: (Vector[W], Either[Vector[E], A]) = (this.warnings, this.value.toRight(this.errors))
}

case class Success[W, A](warnings: Vector[W], result: A) extends Result[Nothing, W, A] {
  override def errors: Vector[Nothing] = Vector.empty

  override def value: Option[A] = Some(result)

  override def run(i: Any): Result[Nothing, W, A] = this

  override def map[B](f: A => B): Validator[Any, Nothing, W, B] = Success(warnings, f(result))

  override def mapWarning[W2](f: W => W2): Validator[Any, Nothing, W2, A] = Success(warnings.map(f), result)

  override def mapError[E2](f: Nothing => E2): Validator[Any, E2, W, A] = this

  override def flatMap[I2 <: Any, E2 >: Nothing, W2 >: W, C](f: A => Validator[I2, E2, W2, C]): Validator[I2, E2, W2, C] = {
    val res = f(result)
    Convert(warnings ++ res.warnings, i2 => res.run(i2))
  }

  override def ignoreWarnings: Validator[Any, Nothing, Nothing, A] = Success(Vector.empty, result)

  override def contramap[I2](f: I2 => Any): Validator[I2, Nothing, W, A] = this
}

case class Error[E, W](warnings: Vector[W], result: Vector[E]) extends Result[E, W, Nothing] {
  override def errors: Vector[E] = result

  override def value: Option[Nothing] = None

  override def run(i: Any): Result[E, W, Nothing] = this

  override def map[B](f: Nothing => B): Validator[Any, E, W, B] = this

  override def mapWarning[W2](f: W => W2): Validator[Any, E, W2, Nothing] =
    Error(warnings.map(f), result)

  override def mapError[E2](f: E => E2): Validator[Any, E2, W, Nothing] =
    Error(warnings, result.map(f))

  override def flatMap[I2 <: Any, E2 >: E, W2 >: W, C](f: Nothing => Validator[I2, E2, W2, C]): Validator[I2, E2, W2, C] = this

  override def ignoreWarnings: Validator[Any, E, Nothing, Nothing] = Error(Vector.empty, result)

  override def contramap[I2](f: I2 => Any): Validator[I2, E, W, Nothing] = this
}

case class Convert[I, E, W, A](warnings: Vector[W], conv: I => Result[E, W, A]) extends Validator[I, E, W, A] {
  override def map[B](f: A => B): Validator[I, E, W, B] =
    Convert(warnings, i => conv(i).map(f).run(null))

  override def mapWarning[W2](f: W => W2): Validator[I, E, W2, A] =
    Convert(warnings.map(f), conv(_).mapWarning(f).run(null))

  override def mapError[E2](f: E => E2): Validator[I, E2, W, A] =
    Convert(warnings, conv(_).mapError(f).run(null))

  override def flatMap[I2 <: I, E2 >: E, W2 >: W, C](f: A => Validator[I2, E2, W2, C]): Validator[I2, E2, W2, C] =
    Convert(warnings, i2 => conv(i2).flatMap(a => f(a).run(i2)).run(null))

  override def run(i: I): Result[E, W, A] = conv(i).run(null)

  override def value: Option[A] = None

  override def errors: Vector[E] = Vector.empty

  override def ignoreWarnings: Validator[I, E, Nothing, A] =
    Convert(Vector.empty, conv(_).ignoreWarnings.run(null))

  override def contramap[I2](f: I2 => I): Validator[I2, E, W, A] =
    Convert(warnings, f andThen conv)
}

object Validator {
  def unit: Validator[Any, Nothing, Nothing, Unit] = Success(Vector.empty, ())

  def from[I]: Validator[I, Nothing, Nothing, I] =
    Convert(Vector.empty, i => success(i))

  def fromFunction[A, B](f: A => B): Validator[A, Nothing, Nothing, B] =
    convert((a: A) => success(f(a)))

  def fromTry[A](tRY: Try[A]): Result[Throwable, Nothing, A] =
    tRY.fold(error, success)

  def fromFallible[A, B](f: A => Try[B]): Validator[A, Throwable, Nothing, B] =
    convert(a => f(a).fold(error, success))

  def success[A](a: A): Result[Nothing, Nothing, A] =
    Success(Vector.empty, a)

  def warning[W](warning: W): Result[Nothing, W, Unit] =
    Success(Vector(warning), ())

  def error[E](error: E): Result[E, Nothing, Nothing] =
    Error(Vector.empty, Vector(error))

  def test[A](pred: A => Boolean): Validator[A, Unit, Nothing, A] =
    Convert(Vector.empty, a =>
      if (pred(a)) success(a)
      else error(())
    )

  def convert[I, E, W, A](conv: I => Result[E, W, A]): Validator[I, E, W, A] =
    Convert(Vector.empty, i => conv(i))

  implicit class Tuple2Ops[I, E, W, A, B](value: (Validator[I, E, W, A], Validator[I, E, W, B])) {
    def join: Validator[I, E, W, (A, B)] = value._1.zip(value._2)
    def mapN[C](f: (A, B) => C): Validator[I, E, W, C] = value.join.map(f.tupled)
    def convertTo[C](f: (A, B) => C): Validator[I, E, W, C] = value.join.map(f.tupled)
  }

  implicit class Tuple3Ops[I, E, W, A, B, C](value: (Validator[I, E, W, A], Validator[I, E, W, B], Validator[I, E, W, C])) {
    def join: Validator[I, E, W, (A, B, C)] = convert { i =>
      val aVal = value._1.run(i)
      val bVal = value._2.run(i)
      val cVal = value._3.run(i)

      aVal.zip(bVal).zip(cVal).map(t => (t._1._1, t._1._2, t._2)).run(null)
    }

    def mapN[D](f: (A, B, C) => D): Validator[I, E, W, D] = value.join.map(f.tupled)
    def convertTo[D](f: (A, B, C) => D): Validator[I, E, W, D] = value.join.map(f.tupled)
  }

  implicit class Tuple3Ops2[I1, I2, I3, E, W, A, B, C](value: (Validator[I1, E, W, A], Validator[I2, E, W, B], Validator[I3, E, W, C])) {
    def andAll: Validator[(I1, I2, I3), E, W, (A, B, C)] = convert { inputs =>
      val aVal = value._1.run(inputs._1)
      val bVal = value._2.run(inputs._2)
      val cVal = value._3.run(inputs._3)

      aVal.and(bVal).and(cVal).map(t => (t._1._1, t._1._2, t._2)).run(null)
    }
  }
}
