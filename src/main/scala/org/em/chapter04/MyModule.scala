package org.em.chapter04

import org.em.{Option, Some, None}

object MyModule {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
}
