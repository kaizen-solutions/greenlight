package io.kaizensolutions.greenlight

import shapeless.Lub

sealed trait Validator[-I, +E, +W, +A] extends Product with Serializable {
  def errors: Vector[E]
  def warnings: Vector[W]
  def value: Option[A]
  def run(i: I): Validator[Any, E, W, A]
  def map[B](f: A => B): Validator[I, E, W, B]
  def mapError[E2](f: E => E2): Validator[I, E2, W, A]
  def flatMap[I2 <: I, E2 >: E, W2 >: W, C](f: A => Validator[I2, E2, W2, C]): Validator[I2, E2, W2, C]
  def zip[I2 <: I, E2 >: E, W2 >: W, B](that: Validator[I2, E2, W2, B]): Validator[I2, E2, W2, (A, B)] = Validator.conversion { (i: I2) =>
    val aVal = this.run(i)
    val bVal = that.run(i)

    Result(
      aVal.warnings ++ bVal.warnings,
      aVal.value.flatMap(a => bVal.value.map(b => (a, b))).toRight(aVal.errors ++ bVal.errors)
    )
  }

  def andThen[E2 >: E, W2 >: W, B](that: Validator[A, E2, W2, B]): Validator[I, E2, W2, B]
  def focusOn[B](f: A => B): Validator[I, E, W, B] = map(f)
  def and[I2 <: I, E2 >: E, W2 >: W, B](that: Validator[I2, E2, W2, B]): Validator[I2, E2, W2, (A, B)] = this.zip(that)

  def or[I2 <: I, E2, W2 >: W, B >: A, EW2](that: Validator[I2, E2, W2, B])(implicit lub: Lub[E, W2, EW2]): Validator[I2, E2, EW2, B] = Validator.conversion { (i: I2) =>
    val aVal: Validator[Any, E, W, A] = this.run(i)

    if (aVal.errors.isEmpty) Result(
      aVal.warnings.map(lub.right),
      aVal.value.toRight(Vector.empty)
    )
    else {
      val bVal: Validator[Any, E2, W2, B] = that.run(i)

      val a: Vector[EW2] = (aVal.warnings ++ bVal.warnings).map(lub.right)
      val b: Vector[EW2] = aVal.errors.map(lub.left)
      val c: Vector[EW2] = a ++ b

      Result(
        c,
        bVal.value.toRight(bVal.errors)
      )
    }
  }

  def withError[E2](error: E2): Validator[I, E2, W, A] = this.mapError(_ => error)
}

case class Result[E, W, A](warnings: Vector[W], result: Either[Vector[E], A]) extends Validator[Any, E, W, A] {
  override def run(i: Any): Validator[Any, E, W, A] = this

  override def map[B](f: A => B): Validator[Any, E, W, B] = Result(warnings, result.map(f))

  override def mapError[E2](f: E => E2): Validator[Any, E2, W, A] = Result(warnings, result.left.map(_.map(f)))

  override def flatMap[I2 <: Any, E2 >: E, W2 >: W, C](f: A => Validator[I2, E2, W2, C]): Validator[I2, E2, W2, C] = {
    val r = result.map(f)
    Result(
      warnings ++ r.map(_.warnings).getOrElse(Vector.empty),
      r.flatMap(v => v.value.toRight(v.errors))
    )
  }

  override def andThen[E2 >: E, W2 >: W, B](that: Validator[A, E2, W2, B]): Validator[Any, E2, W2, B] = {
    val thatRes = result.map(that.run)

    Result(
      warnings ++ thatRes.map(_.warnings).getOrElse(Vector.empty),
      thatRes.flatMap(r => r.value.toRight(r.errors))
    )
  }

  override def errors: Vector[E] = result.left.getOrElse(Vector.empty)

  override def value: Option[A] = result.toOption
}

case class Test[I, W](warnings: Vector[W], pred: I => Boolean) extends Validator[I, Unit, W, I] {
  override def value: Option[I] = None

  override def map[B](f: I => B): Validator[I, Unit, W, B] =
    Convert(warnings, i => run(i).map(f))

  override def flatMap[I2 <: I, E2 >: Unit, W2 >: W, C](f: I => Validator[I2, E2, W2, C]): Validator[I2, E2, W2, C] =
    Convert(warnings, i => run(i).flatMap(i2 => f(i2).run(i)))

  override def run(i: I): Validator[Any, Unit, W, I] =
    if (pred(i)) Result(warnings, Right(i))
    else Result(warnings, Left(Vector(())))

  override def andThen[E2 >: Unit, W2 >: W, B](that: Validator[I, E2, W2, B]): Validator[I, E2, W2, B] =
    Convert(warnings, i => run(i).flatMap(_ => that.run(i)))

  override def mapError[E2](f: Unit => E2): Validator[I, E2, W, I] =
    Convert(warnings, i => run(i).mapError(f))

  override def errors: Vector[Unit] = Vector.empty
}

case class Convert[I, E, W, A](warnings: Vector[W], conv: I => Validator[Any, E, W, A]) extends Validator[I, E, W, A] {
  override def map[B](f: A => B): Validator[I, E, W, B] = Convert(warnings, i => conv(i).map(f))

  override def flatMap[I2 <: I, E2 >: E, W2 >: W, C](f: A => Validator[I2, E2, W2, C]): Validator[I2, E2, W2, C] =
    Convert(warnings, i => conv(i).flatMap(a => f(a).run(i)))

  override def run(i: I): Validator[Any, E, W, A] = conv(i).run()

  override def andThen[E2 >: E, W2 >: W, B](that: Validator[A, E2, W2, B]): Validator[I, E2, W2, B] =
    Convert(warnings, i => conv(i).flatMap(a => that.run(a)))

  override def mapError[E2](f: E => E2): Validator[I, E2, W, A] = Convert(warnings, i => conv(i).mapError(f))

  override def value: Option[A] = None

  override def errors: Vector[E] = Vector.empty
}

object Validator {
  def from[I]: Validator[I, Nothing, Nothing, I] = Convert(Vector.empty, i => success(i))
  def success[A](a: A): Validator[Any, Nothing, Nothing, A] = Result(Vector.empty, Right(a))
  def failure[E](error: E): Validator[Any, E, Nothing, Nothing] = Result(Vector.empty, Left(Vector(error)))
  def test[A](pred: A => Boolean): Validator[A, Unit, Nothing, A] = Test(Vector.empty, pred)
  def conversion[I, E, W, A](conv: I => Validator[Any, E, W, A]): Validator[I, E, W, A] = Convert(Vector.empty, i => conv(i))

  implicit class Tuple2Ops[I, E, W, A, B](value: (Validator[I, E, W, A], Validator[I, E, W, B])) {
    def join: Validator[I, E, W, (A, B)] = value._1.zip(value._2)
    def mapN[C](f: (A, B) => C): Validator[I, E, W, C] = value.join.map(f.tupled)
    def convertTo[C](f: (A, B) => C): Validator[I, E, W, C] = value.join.map(f.tupled)
  }

  implicit class Tuple3Ops[I, E, W, A, B, C](value: (Validator[I, E, W, A], Validator[I, E, W, B], Validator[I, E, W, C])) {
    def join: Validator[I, E, W, (A, B, C)] = conversion { i =>
      val aVal = value._1.run(i)
      val bVal = value._2.run(i)
      val cVal = value._3.run(i)

      Result(
        aVal.warnings ++ bVal.warnings ++ cVal.warnings,
        (
          for {
            a <- aVal.value
            b <- bVal.value
            c <- cVal.value
          } yield (a, b, c)
          ).toRight(aVal.errors ++ bVal.errors ++ cVal.errors)
      )
    }

    def mapN[D](f: (A, B, C) => D): Validator[I, E, W, D] = value.join.map(f.tupled)
    def convertTo[D](f: (A, B, C) => D): Validator[I, E, W, D] = value.join.map(f.tupled)
  }
}
