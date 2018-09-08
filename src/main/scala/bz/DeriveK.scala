package bz

trait NT[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}

trait TransformK[K[_[_]]] {
  def apply[F[_], G[_]](nt: NT[F, G])(ex: K[F]): K[G]
}

case class Iso[A, B](from: A => B, to: B => A)

object DeriveK extends App {
  case class Example[F[_]](f: F[Int], g: F[String])

  type ExRepr[F[_]] = (F[Int], F[String])

  val list2option: NT[List, Option] = new NT[List, Option] {
    def apply[A](fa: List[A]): Option[A] = fa.headOption
  }

  type TupleK[F[_], A, B] = (F[A], F[B])
  def transformTupleK[A, B]: TransformK[({ type G[F[_]] = TupleK[F, A, B] })#G] =
  new TransformK[({ type G[F[_]] = TupleK[F, A, B] })#G] {
    type Out[F[_]] = TupleK[F, A, B]
    def apply[F[_], G[_]](nt: NT[F, G])(ex: Out[F]): Out[G] =
      (nt(ex._1), nt(ex._2))
   }

  println(transformTupleK[Int, String](list2option)((List(1, 2), List.empty[String])))

  def genExample[F[_]]: Iso[Example[F], ExRepr[F]] =
    Iso((x: Example[F]) => (x.f, x.g), (Example.apply[F] _).tupled)

}
