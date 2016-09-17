package com.jgdodson.examples

class Currency(val amt: Double) {

  // Convenience constructor
  def this(amt: Int) = this(amt.toDouble)

  // Add currencies
  def + (rhs: Currency): Currency = {

    Currency(amt + rhs.amt)
  }


  override def toString: String = s"Currency($amt)"

  def mkString: String = f"$$$amt%.2f"
}

object Currency {

  def apply(n: Int): Currency = new Currency(n)

  def apply(n: Double): Currency = new Currency(n)

}
