package com.jgdodson.examples

import scala.math.signum

class Rational (n: Int, d: Int) {

  // Initial conditions
  require(d != 0)

  // Fields
  val (numer, denom) = reduce(n, d)

  // Convenience constructor
  def this(n: Int) = this(n, 1)


  override def toString: String = s"Rational($numer, $denom)"

  def mkString: String = s"$numer/$denom"


  def inv: Rational = Rational(denom, numer)


  def < (rhs: Rational): Boolean =
    numer * rhs.denom < rhs.numer * denom

  def <= (rhs: Rational): Boolean =
    numer * rhs.denom <= rhs.numer * denom


  def > (rhs: Rational): Boolean =
    numer * rhs.denom > rhs.numer * denom

  def >= (rhs: Rational): Boolean =
    numer * rhs.denom >= rhs.numer * denom


  def + (rhs: Rational): Rational =
    Rational(numer * rhs.denom + denom * rhs.numer, denom * rhs.denom)

  def + (rhs: Int): Rational = Rational(numer + denom * rhs, denom)


  def - (rhs: Rational): Rational =
    Rational(numer * rhs.denom - denom * rhs.numer, denom * rhs.denom)

  def - (rhs: Int): Rational = Rational(numer - denom * rhs, denom)


  def * (rhs: Rational): Rational =
    Rational(numer * rhs.numer, denom * rhs.denom)

  def * (rhs: Int): Rational = Rational(numer * rhs, denom)


  def / (rhs: Rational): Rational =
    Rational(numer * rhs.denom, denom * rhs.numer)

  def / (rhs: Int): Rational = Rational(numer, denom * rhs)


  def unary_- : Rational = Rational(-numer, denom)

  def unary_+ : Rational = this


  /** Helper method used to keep invariants on numer and denom
    *
    * gcd: (N x N) \ {(0,0)} --> Z+
    *
    *
    */
  private def gcd(a: Int, b: Int): Int = {

    require(a >= 0 && b > 0 || b >= 0 && a > 0)

    if (b == 0) a else gcd(b, a % b)
  }

  /**
    *
    *  The following invariants are maintained. For a rational r = numer/denom,
    *
    *  1. numer < 0 iff r < 0.
    *  2. denom > 0
    *  3. gcd(numer, denom) == 1
    */
  private def reduce(n: Int, d: Int): (Int, Int) = {

     require(d != 0)

     val g  = gcd(n.abs, d.abs)
     val s  = signum(d)

     ((s * n) / g, (s * d) / g)
  }
}


object Rational {

  // Convenience instantiator
  def apply(n: Int, d: Int): Rational = new Rational(n, d)

  // For the auxiliary constructor
  def apply(n: Int): Rational = new Rational(n)

  def max(lhs: Rational, rhs: Rational): Rational = {
    if (lhs > rhs) lhs else rhs
  }

  def min(lhs: Rational, rhs: Rational): Rational = {
    if (lhs < rhs) lhs else rhs
  }

  // Implicit conversion from Int to Rational
  // implicit def intToRational(x : Int): Rational = Rational(x, 1)
}
