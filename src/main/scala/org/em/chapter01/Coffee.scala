package org.em.chapter01

class Coffee {
  private var _price = 0

  def price = _price

  def price_=(newValue: Int): Unit = {
    _price = newValue
  }

}
