package org.em.chapter01

case class Charge(cc: CreditCard, amount: Int) {

  def combine(other: Charge): Charge = {
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new Exception()
  }
}
