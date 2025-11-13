package edu.luc.cs.laufer.cs371.shapes

import org.scalacheck.{Arbitrary, Gen, Properties, Prop}
import org.scalacheck.Prop.forAll
import cats.laws.discipline.{FunctorTests, TraverseTests}
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration

import structures.*
import structures.ShapeF.*

/**
  * Property-based testing of category theory laws.
  * Tests Functor and Traverse laws using Cats law testing framework.
  */
class TestLaws extends AnyFunSuite with FunSuiteDiscipline with Configuration:

  // Basic equality tests
  test("Rectangle equality"):
    assert(Rectangle(3, 4) == Rectangle(3, 4))
  
  test("Ellipse equality"):
    assert(Ellipse(5, 6) == Ellipse(5, 6))
  
  test("Location equality"):
    assert(Location(1, 2, 10) == Location(1, 2, 10))
  
  test("Group equality"):
    assert(Group(Seq(1, 2, 3)) == Group(Seq(1, 2, 3)))

  test("Rectangle toString"):
    assert(Rectangle(3, 4).toString == "Rectangle(3,4)")
  
  test("Location toString"):
    assert(Location(1, 2, Rectangle(3, 4)).toString.contains("Location"))

  // Generators for creating arbitrary ShapeF instances
  def genRectangle: Gen[ShapeF[Nothing]] = 
    for
      w <- Gen.posNum[Int]
      h <- Gen.posNum[Int]
    yield Rectangle(w, h)
  
  def genEllipse: Gen[ShapeF[Nothing]] = 
    for
      sw <- Gen.posNum[Int]
      sh <- Gen.posNum[Int]
    yield Ellipse(sw, sh)
  
  def genLocation[A](g: Gen[A]): Gen[ShapeF[A]] = 
    for
      x <- Gen.choose(-100, 100)
      y <- Gen.choose(-100, 100)
      shape <- g
    yield Location(x, y, shape)
  
  def genGroup[A](g: Gen[A]): Gen[ShapeF[A]] = 
    for
      size <- Gen.choose(0, 5)
      shapes <- Gen.listOfN(size, g)
    yield Group(shapes.toSeq)

  /**
    * Arbitrary instance for ShapeF.
    * This allows ScalaCheck to generate random ShapeF values for property testing.
    */
  given [A](using Arbitrary[A]): Arbitrary[ShapeF[A]] = Arbitrary:
    val g = Arbitrary.arbitrary[A]
    Gen.oneOf(
      genRectangle,
      genEllipse,
      genLocation(g),
      genGroup(g)
    )

  // Cats Eq instance for testing
  given [A](using cats.Eq[A]): cats.Eq[ShapeF[A]] = cats.Eq.fromUniversalEquals

  /**
    * Test Functor laws for ShapeF.
    * Laws tested:
    * - Identity: fa.map(identity) == fa
    * - Composition: fa.map(f).map(g) == fa.map(f andThen g)
    */
  checkAll("ShapeF.FunctorLaws", FunctorTests[ShapeF].functor[Int, Int, Int])

  /**
    * Test Traverse laws for ShapeF.
    * Laws tested:
    * - Identity: traverse(fa)(identity) == identity(fa)
    * - Sequential composition
    * - Parallel composition (naturality)
    * - Functor laws (Traverse extends Functor)
    * - Foldable laws (Traverse extends Foldable)
    */
  checkAll("ShapeF.TraverseLaws", 
    TraverseTests[ShapeF].traverse[Int, Int, Int, Int, Option, Option])

end TestLaws