package io.kaizensolutions

package object calv1 extends ValidatorSyntax with ResultSyntax {

  /**
   * Extractor to destructure nested tuples in an ergonomic fashion
   * @tparam A
   * @tparam B
   */
  type *[A, B] = (A, B)
  object * {
    def unapply[A, B](arg: (A, B)): Some[(A, B)] = Some(arg)
  }
}
