object App extends App {
  implicit def valueUtils[T](value: T) = new Utils(value)

  class Utils[T](value: T) {
    implicit def print = println(value) }

  trait Iterable[T] {
    def filter(predicate: T => Boolean): Any
  }

  class RichList[T](values: Seq[T]) extends Iterable[T] {
    private val list = values.foldLeft(List[T]()) { (list, elem) => elem :: list }.reverse

    override def toString = list.toString()
    override def filter(predicate: T => Boolean): List[T] = for (e <- list if predicate(e)) yield e
  }
  object RichList {
    def apply[T](values: T*) = new RichList[T](values)
  }

  val list = RichList(1, 2, 3, 4, 5)
  list.filter(_ % 2 == 0).print
}
