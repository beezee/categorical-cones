package bz

object Main extends App {

  sealed trait SC // SourceCategory
  case object A extends SC
  case object B extends SC
  case object C extends SC

  sealed trait Xor[A, B]
  case class Left[A, B](a: A) extends Xor[A, B]
  case class Right[A, B](b: B) extends Xor[A, B]

  type DeltaC = Xor[A.type, Xor[B.type, C.type]]

  object Apex {
    def apply[A <: SC](a: A): Apex = Apex(a match {
      case A => Left[A.type, Xor[B.type, C.type]](A)
      case B => Right[A.type, Xor[B.type, C.type]](Left[B.type, C.type](B))
      case C => Right[A.type, Xor[B.type, C.type]](Right[B.type, C.type](C))
    })

    def fmap[A <: SC, B <: SC](apx: Apex)(fab: A => B): Apex = apx
  }
  case class Apex(abc: DeltaC)

  object Maybe {
    def apply[A <:SC](): Maybe[A] = Nothing[A]
    def apply[A <:SC](a: A): Maybe[A] = Just(a)
    def fmap[A <: SC, B <: SC](ma: Maybe[A])(fab: A => B): Maybe[B] = ma match {
      case Just(a) => Just(fab(a))
      case Nothing() => Nothing[B]()
    }
  }
  sealed trait Maybe[A <: SC]
  case class Nothing[A <: SC]() extends Maybe[A]
  case class Just[A <: SC](a: A) extends Maybe[A]

  def nt[A <: SC](a: A)(apx: Apex): Maybe[A] = a match {
    case A => apx.abc match {
      case Left(x) => Just(x)
      case _ => Nothing[A]
    }
    case B => apx.abc match {
      case Right(Left(x)) => Just(x)
      case _ => Nothing[A]
    }
    case C => apx.abc match {
      case Right(Right(x)) => Just(x)
      case _ => Nothing[A]
    }
  }

  println(nt(A)(Apex(A)))
  println(nt(A)(Apex(B)))
  println(nt(A)(Apex(C)))
  println(nt(B)(Apex(A)))
  println(nt(B)(Apex(B)))
  println(nt(B)(Apex(C)))
  println(nt(C)(Apex(A)))
  println(nt(C)(Apex(B)))
  println(nt(C)(Apex(C)))
}
