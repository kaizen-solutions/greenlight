package io.kaizensolutions.calvin3initial

trait Combine[A] {
  def combine(a1: A, a2: A): A
}

object Combine extends LowPriorityCombine {
  def apply[A](implicit combine: Combine[A]): Combine[A] = combine

  def left[A]: Combine[A]  = (l, _) => l
  def right[A]: Combine[A] = (_, r) => r
}

trait LowPriorityCombine {
  implicit def combineSeq[A]: Combine[Seq[A]] =
    (a1: Seq[A], a2: Seq[A]) => a1 ++ a2
}
