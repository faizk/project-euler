object P29 extends Euler {

  import P25.{toBigInt,toBigDecimal}

  def soln = (for {
    a <- 2 to 100
    b <- 2 to 100
  } yield a pow b).toSet size
}

// vim: set ts=2 sw=2 et:
