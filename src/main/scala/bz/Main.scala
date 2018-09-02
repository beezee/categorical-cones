package bz

trait Morphisms[C] {
  case class `=>`[A <: C, B <: C](a: A, b: B) {
    def pipe[D <: C](implicit bd: (B `=>` D)): `=>`[A, D] =
      `=>`(a, bd.b)
  }
  def apply[A <: C, B <: C](a: A, b: B): `=>`[A, B] = `=>`(a, b)
  def pipe[G <: C, H <: C, I <: C](implicit gh: (G `=>` H), hi: (H `=>` I)): (G `=>` I) =
    `=>`(gh.a, hi.b)
}

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

object Duals extends App {

  sealed trait Cat
  case class AA() extends Cat
  case class A() extends Cat
  case class B() extends Cat
  case class FAB() extends Cat
  case class C() extends Cat
  case class D() extends Cat
  case class E() extends Cat
  case class FF() extends Cat
  case class G() extends Cat

  val mph = new Morphisms[Cat] {}

  object initialEncoding {
    implicit val aaa = mph(AA(), A())
    implicit val afab = mph(A(), FAB())
    implicit val bfab = mph(B(), FAB())
    implicit val fabc = mph(FAB(), C())
    implicit val fabd = mph(FAB(), D())
    implicit val fabe = mph(FAB(), E())
    implicit val cff = mph(C(), FF())
    implicit val dff = mph(D(), FF())
    implicit val cg = mph(C(), G())
    implicit val dg = mph(D(), G())
    implicit val eg = mph(E(), G())
  }

  object finalEncoding {
    implicit val aaa = mph(A(), AA())
    implicit val faba = mph(FAB(), A())
    implicit val fabb = mph(FAB(), B())
    implicit val cfab = mph(C(), FAB())
    implicit val dfab = mph(D(), FAB())
    implicit val efab = mph(E(), FAB())
    implicit val ffc = mph(FF(), C())
    implicit val ffd = mph(FF(), D())
    implicit val gc = mph(G(), C())
    implicit val gd = mph(G(), D())
    implicit val ge = mph(G(), E())
  }

  {
    import initialEncoding._
    println("A => FAB pipe FAB => C = " ++ mph.pipe[A, FAB, C].toString)
    println("AA => A pipe FAB => C pipe C => FF = " ++ mph.pipe[AA, A, FAB].pipe[C].pipe[FF].toString)
  }
}
