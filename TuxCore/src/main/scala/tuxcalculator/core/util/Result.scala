package tuxcalculator.core.util

sealed trait Result[+T] {
  
  def ~ [U](func: T => U): Result[U]
  def ~> [U](func: T => Result[U]): Result[U]
  def ~@ (line: => String): Result[T]
  
  def map[U](func: T => U): Result[U]
  def flatMap[U](func: T => Result[U]): Result[U]
  def trace(line: => String): Result[T]
}

object Result {
  
  def and[A,B](r1: => Result[A], r2: => Result[B]): Result[(A, B)] = r1 ~> (a => r2 ~ (b => (a, b)))
  def and[A,B,C](r1: => Result[A], r2: => Result[B], r3: => Result[C]): Result[(A, B, C)] = r1 ~> (a => r2 ~> (b => r3 ~ (c => (a, b, c))))
  def and[A,B,C,D](r1: => Result[A], r2: => Result[B], r3: => Result[C], r4: => Result[D]): Result[(A, B, C, D)] = r1 ~> (a => r2 ~> (b => r3 ~> (c => r4 ~ (d => (a, b, c, d)))))
  
  case class Value[T](value: T) extends Result[T] {
    override def ~ [U](func: T => U): Value[U] = map(func)
    override def ~>[U](func: T => Result[U]): Result[U] = flatMap(func)
    override def ~@ (line: => String): this.type = this
    
    override def map[U](func: T => U): Value[U] = Value(func(value))
    override def flatMap[U](func: T => Result[U]): Result[U] = func(value)
    override def trace(line: => String): this.type = this
  }

  case class Error(msg: String, trace: Vector[String] = Vector()) extends Result[Nothing] {
    override def ~ [U](func: Nothing => U): this.type = this
    override def ~> [U](func: Nothing => Result[U]): this.type = this
    override def ~@ (line: => String): Error = trace(line)

    override def map[U](func: Nothing => U): this.type = this
    override def flatMap[U](func: Nothing => Result[U]): this.type = this
    override def trace(line: => String): Error = Error(msg, trace.appended(line))
  }
}
