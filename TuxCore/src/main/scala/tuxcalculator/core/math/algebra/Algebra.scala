package tuxcalculator.core.math.algebra

object Algebra {

  trait Monoid[T] {
    def neutral: T
    def combine(a: T, b: T): T
  }
  
  trait Group[T] extends Monoid[T] {
    def inverse(a: T): T
  }
  
  trait Ring[T] {
    def zero: T
    def one: T
    def negate(a: T): T
    def add(a: T, b: T): T
    def multiply(a: T, b: T): T
    
    def subtract(a: T, b: T): T = add(a, negate(b))
    
    val addition: Group[T] = new Group[T] {
      override def neutral: T = zero
      override def inverse(a: T): T = negate(a)
      override def combine(a: T, b: T): T = add(a, b)
    }
    val multiplication: Monoid[T] = new Monoid[T] {
      override def neutral: T = one
      override def combine(a: T, b: T): T = multiply(a, b)
    }
  }
  
  trait ModularRing[T] extends Ring[T] {
    def divMod(a: T, b: T): (T, T)
  }
  
  trait Field[T] extends Ring[T] {
    def invert(a: T): T
    
    def divide(a: T, b: T): T = multiply(a, invert(b))
    
    override val multiplication: Group[T] = new Group[T] {
      override def neutral: T = one
      override def inverse(a: T): T = invert(a)
      override def combine(a: T, b: T): T = multiply(a, b)
    }
  }
  
  trait OrderedField[T] extends Field[T] {
    val ordering: Ordering[T]
    def compare(a: T, b: T): Int = ordering.compare(a, b)
  }
}
