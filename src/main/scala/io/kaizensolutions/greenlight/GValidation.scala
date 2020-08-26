package io.kaizensolutions.greenlight

import cats.data.{ Ior, IorNec }
import cats.kernel.Semigroup
import cats.syntax.semigroup._
import io.kaizensolutions.greenlight.GValidation._

case class GValidation[-I, +E: Semigroup, +O](run: I => Ior[E, O]) { self =>
  def andThen[I1 <: I, E1 >: E: Semigroup, O1 >: O, OO](
    fn: GValidation[O, E1, OO]
  ): GValidation[I1, E1, OO] =
    GValidation(run(_).flatMap(fn.run))

  def andThenPropagate[I1 <: I, E1 >: E: Semigroup, O1 >: O, OO](
    fn: GValidation[O, E1, OO]
  ): GValidation[I1, E1, (O, OO)] =
    GValidation { a =>
      for {
        b <- run(a)
        c <- fn.run(b)
      } yield (b, c)
    }

  def >>>[I1 <: I, E1 >: E: Semigroup, O1 >: O, OO](fn: GValidation[O, E1, OO]): GValidation[I1, E1, OO] =
    andThen(fn)

  def >:>[I1 <: I, E1 >: E: Semigroup, O1 >: O, OO](
    fn: GValidation[O, E1, OO]
  ): GValidation[I1, E1, (O, OO)] =
    andThenPropagate(fn)

  def or[I1 <: I, E1 >: E: Semigroup, O1 >: O](
    fallbackValidation: GValidation[I1, E1, O1]
  ): GValidation[I1, E1, O1] =
    GValidation(a =>
      run(a) match {
        case Ior.Left(_) => fallbackValidation.run(a)
        case ok          => ok
      }
    )

  /**
   * Provide applicative composition for independent computations but preserve Ior (monadic) semantics for dependent
   * computations
   *
   * @param that is the other validation that you want to combine
   * @param combiner specifies how to combine the output
   * @tparam I1
   * @tparam E1
   * @tparam O1
   * @tparam O2
   * @tparam O3
   * @return
   */
  def zipWith[I1 <: I, E1 >: E: Semigroup, O1 >: O, O2, O3](
    that: GValidation[I1, E1, O2]
  )(combiner: (O, O2) => O3): GValidation[I1, E1, O3] =
    GValidation(a =>
      (self.run(a), that.run(a)) match {
        case (Ior.Right(a), Ior.Right(b))       => Ior.right(combiner(a, b))
        case (Ior.Right(a), Ior.Both(e, b))     => Ior.both(e, combiner(a, b))
        case (Ior.Both(e, a), Ior.Right(b))     => Ior.both(e, combiner(a, b))
        case (Ior.Both(eA, a), Ior.Both(eB, b)) => Ior.both(convert[E, E1](eA) |+| eB, combiner(a, b))
        case (Ior.Left(eA), Ior.Both(eB, _))    => Ior.left(convert[E, E1](eA) |+| eB)
        case (Ior.Both(eA, _), Ior.Left(eB))    => Ior.left(convert[E, E1](eA) |+| eB)
        case (Ior.Left(eA), Ior.Left(eB))       => Ior.left(convert[E, E1](eA) |+| eB)
        case (Ior.Left(eA), Ior.Right(_))       => Ior.left(eA)
        case (Ior.Right(_), Ior.Left(eB))       => Ior.left(eB)
      }
    )

  def and[I1 <: I, E1 >: E: Semigroup, O1 >: O, O2](
    that: GValidation[I1, E1, O2]
  ): GValidation[I1, E1, (O1, O2)] = self.zipWith(that)((b: O1, c: O2) => (b, c))

  def +[I1 <: I, E1 >: E: Semigroup, O1 >: O, O2](
    that: GValidation[I1, E1, O2]
  ): GValidation[I1, E1, (O1, O2)] =
    self and that

  def map[O1 >: O, O2](fn: O1 => O2): GValidation[I, E, O2] =
    GValidation(run(_).map(fn))

  def flatMap[I1 <: I, E1 >: E: Semigroup, C](fn: O => GValidation[I1, E1, C]): GValidation[I1, E1, C] =
    GValidation { a =>
      run(a).flatMap { b =>
        fn(b).run(a)
      }
    }

  def contramap[I1](fn: I1 => I): GValidation[I1, E, O] = GValidation(i1 => run(fn(i1)))

  def as[O1](c: O1): GValidation[I, E, O1] = GValidation(run(_).map(_ => c))
}

object GValidation {
  def validate[I, E: Semigroup, O](fn: I => Ior[E, O]): GValidation[I, E, O] = GValidation(fn)

  implicit class ProductValidationOps[I, E: Semigroup, O](p: GValidation[I, E, O]) {
    def preserveInput: GValidation[I, E, I] = GValidation(a => p.run(a).map(_ => a))
  }

  implicit class FailOps[A: Semigroup](a: A) {
    def fail: Ior[A, Nothing]       = Ior.left(a)
    def failNec: IorNec[A, Nothing] = Ior.leftNec(a)
  }

  implicit class WarningOps[A, B](val t: (A, B)) extends AnyVal {
    def succeedWarn: Ior[A, B] = Ior.both(t._1, t._2)
  }

  implicit class SucceedOps[A](val a: A) extends AnyVal {
    def succeed: Ior[Nothing, A]    = Ior.right(a)
    def succeedNec[E]: IorNec[E, A] = Ior.right(a)
  }

  implicit class GValidationOps[I, E: Semigroup, O](raw: I => Ior[E, O]) {
    def lift: GValidation[I, E, O] = GValidation(raw)
  }

  type ***[A, B] = (A, B)
  object *** {
    def unapply[A, B](arg: (A, B)): Some[(A, B)] = Some(arg)
  }

  private def convert[A, B](a: A)(implicit evidence: A <:< B): B = evidence(a)
}
