package tuxcalculator.core.util

sealed trait Result[+T] {
  
  def ~ [U](func: T => U): Result[U] = map(func)
  def ~> [U](func: T => Result[U]): Result[U] = flatMap(func)
  
  def map[U](func: T => U): Result[U]
  def flatMap[U](func: T => Result[U]): Result[U]
}

object Result {
  
  def and[A,B](r1: => Result[A], r2: => Result[B]): Result[(A, B)] = r1 ~> (a => r2 ~ (b => (a, b)))
  def and[A,B,C](r1: => Result[A], r2: => Result[B], r3: => Result[C]): Result[(A, B, C)] = r1 ~> (a => r2 ~> (b => r3 ~ (c => (a, b, c))))
  def and[A,B,C,D](r1: => Result[A], r2: => Result[B], r3: => Result[C], r4: => Result[D]): Result[(A, B, C, D)] = r1 ~> (a => r2 ~> (b => r3 ~> (c => r4 ~ (d => (a, b, c, d)))))
  
  case class Value[T](value: T) extends Result[T] {
    override def map[U](func: T => U): Result[U] = Value(func(value))
    override def flatMap[U](func: T => Result[U]): Result[U] = func(value)
  }

  case class Error(msg: String) extends Result[Nothing] {
    override def map[U](func: Nothing => U): Result[U] = this
    override def flatMap[U](func: Nothing => Result[U]): Result[U] = this
  }
}
