package u02.ex
import u02.AlgebraicDataTypes.Person
import u02.AlgebraicDataTypes.Person.Teacher
import u02.AlgebraicDataTypes.Person.Student
import u03.Lists.*
import u02.Optionals.*

import scala.annotation.tailrec


object Tasks extends App {

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match
    case (List.Nil(), _) => List.Nil()
    case (List.Cons(h, t), n) => if n == 0 then List.Cons(h, t) else drop(t, n - 1)


  def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
    case (List.Nil(), right) => right
    case (List.Cons(h, List.Nil()), right) => List.Cons(h, right)
    case (List.Cons(h, t), right) => append(List.Cons(h, List.Nil()), append(t, right))



  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
    case List.Cons(h, t) => append(f(h), flatMap(t)(f))
    case _ => List.Nil()

  def map[A, B](l: List[A])(mapper: A => B): List[B] = flatMap(l)(x => List.Cons(mapper(x), List.Nil()))

  def filter[A](l1: List[A])(pred: A => Boolean): List[A] = flatMap(l1)(p => if pred(p) then List.Cons(p, List.Nil()) else List.Nil())

  @tailrec
  def max(l: List[Int]): Option[Int] = l match
    case List.Nil() => Option.None()
    case List.Cons(h, List.Nil()) => Option.Some(h)
    case List.Cons(h, List.Cons(h2, t2)) => if h >= h2 then max(List.Cons(h,t2)) else max(List.Cons(h2,t2))


  def getCourseFromPersonsV1(l: List[Person]): List[String] = map(filter(l)(x => x match
    case Person.Teacher(n,c) => true
    case _ => false
  ))(x => x match
    case Person.Teacher(_, c) => c)

  def getCourseFromPersons(l: List[Person]): List[String] = flatMap[Person, String](l)(x => x match
    case Person.Teacher(_, c) => List.Cons(c, List.Nil())
    case _ => List.Nil()
    )

  @tailrec
  def foldLeft[A, B](list: List[A])(d: B)(acc: (B, A) => B): B = list match
    case List.Nil() => d
    case List.Cons(h, t) => foldLeft(t)(acc(d, h))(acc)

  def foldRight[A,B](list: List[A])(d: B)(acc: (A, B) => B): B = list match
    case List.Nil() => d
    case List.Cons(h, t) => acc(h, foldRight(t)(d)(acc))


  val lst = List.Cons(3, List.Cons(7, List.Cons(1, List.Cons(5, List.Nil()))))
  println(foldLeft(lst)(0)(_ - _)) // -16
  println(foldRight(lst)(0)(_ - _)) // -8




}
