package io.kaizensolutions.nigelv1

import io.kaizensolutions.nigelv1.Validator._
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
    convert(i => this.run(i).flatMap(that.run))

  def zip[I2 <: I, E2 >: E, W2 >: W, B](that: Validator[I2, E2, W2, B]): Validator[I2, E2, W2, (A, B)] =
    convert { i =>
      val aVal = this.run(i)
      val bVal = that.run(i)

      aVal zip bVal
    }

  def focusOn[B](f: A => B): Validator[I, E, W, B] =
    map(f)

  def and[I2, E2 >: E, W2 >: W, B](that: Validator[I2, E2, W2, B]): Validator[(I, I2), E2, W2, (A, B)] =
    convert { (tuple: (I, I2)) =>
      val aVal = this.run(tuple._1)
      val bVal = that.run(tuple._2)

      aVal zip bVal
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

case class Convert[I, E, W, A](warnings: Vector[W], conv: I => Result[E, W, A]) extends Validator[I, E, W, A] {
  override def map[B](f: A => B): Validator[I, E, W, B] =
    Convert(warnings, i => conv(i).map(f))

  override def mapWarning[W2](f: W => W2): Validator[I, E, W2, A] =
    Convert(warnings.map(f), conv(_).mapWarning(f))

  override def mapError[E2](f: E => E2): Validator[I, E2, W, A] =
    Convert(warnings, conv(_).mapError(f))

  override def flatMap[I2 <: I, E2 >: E, W2 >: W, C](f: A => Validator[I2, E2, W2, C]): Validator[I2, E2, W2, C] =
    Convert(warnings, i2 => conv(i2).flatMap(a => f(a).run(i2)))

  override def run(i: I): Result[E, W, A] = conv(i)

  override def value: Option[A] = None

  override def errors: Vector[E] = Vector.empty

  override def ignoreWarnings: Validator[I, E, Nothing, A] =
    Convert(Vector.empty, conv(_).ignoreWarnings)

  override def contramap[I2](f: I2 => I): Validator[I2, E, W, A] =
    Convert(warnings, f andThen conv)
}

object Validator {
  def fromFunction[A, B](f: A => B): Validator[A, Nothing, Nothing, B] =
    convert((a: A) => Result.success(f(a)))

  def success[A](value: A): Validator[Any, Nothing, Nothing, A] =
    fromFunction(Function.const(value))

  def unit: Validator[Any, Nothing, Nothing, Unit] = success(())

  def from[I]: Validator[I, Nothing, Nothing, I] =
    fromFunction(identity[I])

  def fromFallible[A, B](f: A => Try[B]): Validator[A, Throwable, Nothing, B] =
    convert(a => f(a).fold(Result.error, Result.success))

  def test[A](pred: A => Boolean): Validator[A, Unit, Nothing, A] =
    convert(a =>
      if (pred(a)) Result.success(a)
      else Result.error(())
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

      aVal.zip(bVal).zip(cVal).map(t => (t._1._1, t._1._2, t._2))
    }

    def mapN[D](f: (A, B, C) => D): Validator[I, E, W, D] = value.join.map(f.tupled)
    def convertTo[D](f: (A, B, C) => D): Validator[I, E, W, D] = value.join.map(f.tupled)
  }

  implicit class Tuple3Ops2[I1, I2, I3, E, W, A, B, C](value: (Validator[I1, E, W, A], Validator[I2, E, W, B], Validator[I3, E, W, C])) {
    def andAll: Validator[(I1, I2, I3), E, W, (A, B, C)] = convert { inputs =>
      val aVal = value._1.run(inputs._1)
      val bVal = value._2.run(inputs._2)
      val cVal = value._3.run(inputs._3)

      aVal.zip(bVal).zip(cVal).map(t => (t._1._1, t._1._2, t._2))
    }
  }
}
