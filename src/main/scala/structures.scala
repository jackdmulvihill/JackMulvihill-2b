package edu.luc.cs.laufer.cs371.shapes

import cats.{Applicative, Eq, Functor, Show, Traverse}
import cats.Eval
import higherkindness.droste.data.Fix
import higherkindness.droste.util.DefaultTraverse

/**
  * F-algebra based representation of shapes using recursion schemes.
  * ShapeF is the endofunctor, and Shape is its least fixpoint.
  */
object structures:

  /**
    * Non-recursive functor ShapeF for representing different kinds of shapes.
    * Endofunctor for shapes F-algebra.
    * Note that `A` represents the recursive positions in the structure.
    * 
    * @tparam A argument of the endofunctor (recursive positions)
    */
  enum ShapeF[+A] derives CanEqual:
    case Rectangle(width: Int, height: Int) extends ShapeF[Nothing]
    case Ellipse(semiWidth: Int, semiHeight: Int) extends ShapeF[Nothing]
    case Location(x: Int, y: Int, shape: A)
    case Group(shapes: Seq[A])

  import ShapeF.*

  /**
    * Functor instance for ShapeF (declared as given).
    * This allows us to map over the recursive positions.
    */
  given shapeFFunctor: Functor[ShapeF] = new Functor[ShapeF]:
    override def map[A, B](fa: ShapeF[A])(f: A => B): ShapeF[B] = fa match
      case r @ Rectangle(_, _) => r
      case e @ Ellipse(_, _) => e
      case Location(x, y, shape) => Location(x, y, f(shape))
      case Group(shapes) => Group(shapes.map(f))
  end shapeFFunctor

  /**
    * Traverse instance for ShapeF (declared as given).
    * This allows us to traverse the recursive positions.
    */
  given shapeFTraverse: Traverse[ShapeF] = new Traverse[ShapeF]:
    override def traverse[G[_]: Applicative, A, B](fa: ShapeF[A])(f: A => G[B]): G[ShapeF[B]] =
      import cats.syntax.all.*
      fa match
        case Rectangle(w, h) =>
          Applicative[G].pure(Rectangle(w, h))
        case Ellipse(sw, sh) =>
          Applicative[G].pure(Ellipse(sw, sh))
        case Location(x, y, a) =>
          f(a).map(b => Location(x, y, b))
        case Group(shapes) =>
          shapes.toList.traverse(f).map(bs => Group(bs.toSeq))

    // Implement foldLeft and foldRight using DefaultTraverse
    override def foldLeft[A, B](fa: ShapeF[A], b: B)(f: (B, A) => B): B =
      fa match
        case Rectangle(_, _) => b
        case Ellipse(_, _) => b
        case Location(_, _, a) => f(b, a)
        case Group(shapes) => shapes.foldLeft(b)(f)
    
    override def foldRight[A, B](fa: ShapeF[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa match
        case Rectangle(_, _) => lb
        case Ellipse(_, _)   => lb
        case Location(_, _, a) => f(a, lb)
        case Group(shapes) =>
        // Use lazy foldRight — defer each step, not the whole sequence
          shapes.foldRight(lb)((a, acc) => Eval.defer(f(a, acc)))
  /**
    * Eq instance for ShapeF (shapeFEqual).
    */
  given shapeFEqual[T](using Eq[T]): Eq[ShapeF[T]] = Eq.fromUniversalEquals

  /**
    * Show instance for ShapeF (shapeFShow).
    */
  given shapeFShow[T](using Show[T]): Show[ShapeF[T]] = Show.fromToString

  /**
    * Least fixpoint of ShapeF as carrier object for the initial algebra.
    * Shape is defined by applying Fix to ShapeF.
    */
  type Shape = Fix[ShapeF]

  /** Enable typesafe equality for Shape. */
  given CanEqual[Shape, Shape] = CanEqual.derived

  /**
    * Factory functions for creating Shape instances.
    * Lower-case factory function for each kind of shape.
    */
  object shape:
    def rectangle(width: Int, height: Int): Shape = 
      Fix(Rectangle(width, height))
    
    def ellipse(semiWidth: Int, semiHeight: Int): Shape = 
      Fix(Ellipse(semiWidth, semiHeight))
    
    def location(x: Int, y: Int, s: Shape): Shape = 
      Fix(Location(x, y, s))
    
    def group(shapes: Shape*): Shape = 
      Fix(Group(shapes.toSeq))
  end shape

  given Eq[Shape] = Eq.fromUniversalEquals

  given Show[Shape] = Show.fromToString

end structures