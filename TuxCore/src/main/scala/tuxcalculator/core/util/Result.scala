package tuxcalculator.core.util

import scala.language.implicitConversions

sealed trait Result[+T] {
  
  def ~ [U](func: T => U): Result[U]
  def ~> [U](func: T => Result[U]): Result[U]
  def ~@ (line: => String): Result[T]
  
  def map[U](func: T => U): Result[U]
  def flatMap[U](func: T => Result[U]): Result[U]
  def filter(func: T => Boolean, error: String): Result[T]
  def trace(line: => String): Result[T]
  def either: Either[T, Result.Error]
  def eitherUnwrap: Either[T, String]
}

object Result {

  implicit class ImplicitResultResultFlatten[T](result: Result[Result[T]]) {
    def flatten: Result[T] = result.flatMap(identity)
  }

  implicit class ImplicitResultOptionFlatten[T](result: Result[Option[T]]) {
    def flatten(orElse: Result[T]): Result[T] = result.flatMap(opt => opt.map(Result.Value.apply).getOrElse(orElse))
  }

  def and[A,B](r1: => Result[A], r2: => Result[B]): Result[(A, B)] = r1 ~> (a => r2 ~ (b => (a, b)))
  def and[A,B,C](r1: => Result[A], r2: => Result[B], r3: => Result[C]): Result[(A, B, C)] = r1 ~> (a => r2 ~> (b => r3 ~ (c => (a, b, c))))
  def and[A,B,C,D](r1: => Result[A], r2: => Result[B], r3: => Result[C], r4: => Result[D]): Result[(A, B, C, D)] = r1 ~> (a => r2 ~> (b => r3 ~> (c => r4 ~ (d => (a, b, c, d)))))
  
  case class Value[T](value: T) extends Result[T] {
    override def ~ [U](func: T => U): Value[U] = map(func)
    override def ~>[U](func: T => Result[U]): Result[U] = flatMap(func)
    override def ~@ (line: => String): this.type = this
    
    override def map[U](func: T => U): Value[U] = Value(func(value))
    override def flatMap[U](func: T => Result[U]): Result[U] = func(value)
    override def filter(func: T => Boolean, error: String): Result[T] = if (func(value)) this else Error(error)
    override def trace(line: => String): this.type = this
    override def either: Left[T, Nothing] = Left(value)
    override def eitherUnwrap: Left[T, Nothing] = Left(value)
  }

  case class Error(msg: String, trace: Vector[String] = Vector()) extends Result[Nothing] {
    override def ~ [U](func: Nothing => U): this.type = this
    override def ~> [U](func: Nothing => Result[U]): this.type = this
    override def ~@ (line: => String): Error = trace(line)

    override def map[U](func: Nothing => U): this.type = this
    override def flatMap[U](func: Nothing => Result[U]): this.type = this
    override def filter(func: Nothing => Boolean, error: String): this.type = this
    override def trace(line: => String): Error = Error(msg, trace.appended(line))
    override def either: Right[Nothing, Result.Error] = Right(this)
    override def eitherUnwrap: Right[Nothing, String] = Right(msg)
  }
}
