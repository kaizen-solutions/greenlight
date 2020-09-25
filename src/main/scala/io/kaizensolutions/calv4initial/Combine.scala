package io.kaizensolutions.calv4initial

trait Combine[A] {
  def combine(a1: A, a2: A): A
}

object Combine extends LowPriorityCombine {
  def apply[A](implicit combine: Combine[A]): Combine[A] = combine

  def left[A]: Combine[A]  = (l, _) => l
  def right[A]: Combine[A] = (_, r) => r

  implicit def combineOption[A: Combine]: Combine[Option[A]] = { (optA1, optA2) =>
    val both = for {
      a1 <- optA1
      a2 <- optA2
    } yield Combine[A].combine(a1, a2)

    both
      .orElse(optA2)
      .orElse(optA1)
  }
}

trait LowPriorityCombine {
  implicit def combineSeq[A]: Combine[Seq[A]] =
    (a1: Seq[A], a2: Seq[A]) => a1 ++ a2
}
