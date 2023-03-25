package u02

import org.junit.*
import org.junit.Assert.*
import u02.AlgebraicDataTypes.*;
import u03.Lists
import u03.Lists.List
import List.*

import scala.annotation.tailrec

object Tasks {
  import u02.Optionals.*

  //Task 1, svolto da solo

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match
    case (Cons(h, t), n) => if n == 0 then Cons(h, t) else drop(t, n - 1)
    case (Nil(), _) => Nil()

  def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
    case (Cons(h, Nil()), right) => Cons(h, right)
    case (Cons(h, t), right) => append(Cons(h, Nil()), append(t, right))
    case (_, right) => right

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
    case Cons(h, t) => append(f(h), flatMap(t)(f))
    case _ => Nil()

  def map[A, B](l: List[A])(mapper: A => B): List[B] = flatMap(l)(x => Cons(mapper(x), Nil()))

  def filter[A](l1: List[A])(pred: A => Boolean): List[A] = flatMap(l1)(p => if pred(p) then Cons(p, Nil()) else Nil())

  @tailrec
  def max(l: List[Int]): Option[Int] = l match
    case Cons(h, Cons(h2, t2)) => if h >= h2 then max(Cons(h, t2)) else max(Cons(h2, t2))
    case Cons(h, Nil()) => Option.Some(h)
    case _ => Option.None()

  //Task 2, svolto da solo

  def getCourseFromPersons(l: List[Person]): List[String] = flatMap[Person, String](l)(x => x match
    case Person.Teacher(_, c) => Cons(c, Nil())
    case _ => Nil()
  )

  //alternativa con filter + map
  /*
    def getCourseFromPersonsV1(l: List[Person]): List[String] = map(filter(l)(x => x match
      case Person.Teacher(n, c) => true
      case _ => false
      )
    )(x => x match
      case Person.Teacher(_, c) => c)
  */

  @tailrec
  def foldLeft[A, B](list: List[A])(d: B)(acc: (B, A) => B): B = list match
    case Cons(h, t) => foldLeft(t)(acc(d, h))(acc)
    case _ => d


  def foldRight[A, B](list: List[A])(d: B)(acc: (A, B) => B): B = list match
    case Cons(h, t) => acc(h, foldRight(t)(d)(acc))
    case _ => d

}

//Task 3, svolto da solo

//addtional code was copied from u03.Streams to keep the additional streams methods in their original module
object TasksStream:

  import Lists.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): List[A] = stream match
      case Cons(h, t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def drop[A](stream: Stream[A])(n : Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n)  => if n > 0 then drop(tail())(n - 1) else Cons(head, tail)
      case _ => Empty()

    def constant[A](k: A): Stream[A] = Stream.iterate(k)(x => x)

    val fibs: Stream[Int] =
      def fibonacciMeth(prev: Option[Int], curr: Int): Stream[Int] = (prev, curr) match
        case (None, 0) => Stream.cons(0, fibonacciMeth(Some(0), 0))
        case (Some(0), 0) => Stream.cons(1, fibonacciMeth(Some(0), 1))
        case (Some(p), c) => Stream.cons(p + c, fibonacciMeth(Some(c), p + c))
      fibonacciMeth(None, 0)


    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

  end Stream


class DropTest {
  import Tasks.drop;

  val lst = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testDropEmptyList() =
    assertEquals(drop(Nil(), 1), Nil())

  @Test def testDropMultipleElements() =
    assertEquals(drop(lst, 3), Nil())

  @Test def testDropMultipleTimes() =
    assertEquals(drop(drop(lst, 1), 1), Cons(30, Nil()))
}


class AppendTest {
  import Tasks.append;

  val lst1 = Cons(10, Cons(20, Nil()))
  val lst2 = Cons(40, Nil())

  @Test def testAppendToEmptyList() =
    assertEquals(append(Nil(), lst1 ), lst1)

  @Test def testAppendLists() =
    assertEquals(append(lst1, lst2), Cons(10, Cons(20, Cons(40, Nil()))))

  @Test def testAppendEmptyLists() =
    assertEquals(append(Nil(), Nil()), Nil())

}

class flatMapTest {
  import Tasks.flatMap;
  import Tasks.map;
  import Tasks.filter;

  val toDoubleUp = x => Cons(x, Cons(x, Nil()));
  val toMultiple: Int => Int =  _ * 2
  val greaterThan10predicate: Int => Boolean = _ > 10


  val lst  = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testFlatMapDoubleUp() =
    assertEquals(flatMap(lst)(toDoubleUp), Cons(10, Cons(10, Cons(20, Cons(20, Cons(30, Cons(30, Nil())))))))

  @Test def testMapToMultiple() =
    assertEquals(map(lst)(toMultiple), Cons(20, Cons(40, Cons(60, Nil()))))

  @Test def testFilterGreaterThan() =
    assertEquals(filter(lst)(greaterThan10predicate), Cons(20, Cons(30, Nil())))

}

class maxTest {
  import Tasks.max;
  import Optionals.Option;

  val lst = Cons(35, Cons(20, Cons(25, Nil())))

  @Test def testMaxOfEmptyList() =
    assertEquals(max(Nil()), Option.None())

  @Test def testMaxList() =
    assertEquals(max(lst), Option.Some(35))
 }

class CoursesTeacherTest {
  import Tasks.getCourseFromPersons;

  val lst = Cons(Person.Teacher("bob", "math"), Cons(Person.Student("steve",4), Cons(Person.Teacher("james","eng"), List.Nil())))

  @Test def testCoursesFromTeacher() =
    assertEquals(getCourseFromPersons(lst), Cons("math", Cons("eng", Nil())))

  @Test def testNoTeachersInList() =
    assertEquals(getCourseFromPersons(Cons(Person.Student("steve", 4), Nil())), Nil())
}

class FoldingTest {
  import Tasks.foldLeft;
  import Tasks.foldRight;

  val lst = Cons (3, Cons(7, Cons(1, Cons(5, Nil()))))
  val startValue = 1;
  val productAcc: (Int, Int) => Int = _ * _
  val leftProduct = ((((startValue * 3) * 7) * 1 )* 5);
  val rightProduct = (3 * (7 * (1 * (5 * startValue))));

  @Test def testSimpleFoldLeft() =
    assertEquals(foldLeft(lst)(startValue)(_ * _), leftProduct)

  @Test def testSimpleFoldRight() =
    assertEquals(foldLeft(lst)(startValue)(_ * _), rightProduct)

}

class StreamTest {
  import TasksStream.Stream.*;

  val exStream = iterate(10)(_ + 10);
  val streamLimit = 3;
  val dropNumber = 5

  val constValue = 1;

  @Test def testDropFromStream() =
    assertEquals(toList(take(drop(exStream)(dropNumber))(streamLimit)), Cons(60, Cons(70, Cons(80, Nil()))))

  @Test def testConstantStream() =
    assertEquals(take(constant(constValue))(streamLimit), Cons(1, Cons(1, Cons(1, Nil()))))

}

class FibonacciTest {
  import TasksStream.Stream.*;

  val numOfCollect = 10;
  val fibFirstElements = Cons(0,Cons(1,Cons(1,Cons(2,Cons(3,Cons(5,Cons(8,Cons(13,Cons(21,Cons(34,Nil()))))))))));

  @Test def testFibonacciFewElements() =
  assertEquals(toList(take(fibs)(numOfCollect)), fibFirstElements)

  @Test def testEmptyFibonacciSequence() =
    assertEquals(toList(take(fibs)(0)), Nil())
}




