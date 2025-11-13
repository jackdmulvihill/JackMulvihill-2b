package edu.luc.cs.laufer.cs371.shapes

import org.scalatest.funsuite.AnyFunSuite
import higherkindness.droste.scheme
import higherkindness.droste.data.Fix

import TestFixtures.*
import structures.*
import behaviors.*

/**
  * Apply algebras using cata function.
  * Test suite adapted from the original project.
  */
class TestBoundingBox extends AnyFunSuite:

  def testBoundingBox(description: String, s: Shape, x: Int, y: Int, width: Int, height: Int): Unit =
    test(description):
      // Apply boundingBox algebra using scheme.cata
      // Result is Fix(Location(x, y, Fix(Rectangle(w, h))))
      val result = scheme.cata(boundingBox).apply(s)
      result match
        case Fix(ShapeF.Location(u, v, Fix(ShapeF.Rectangle(w, h)))) =>
          assert(x == u, s"Expected x=$x but got $u")
          assert(y == v, s"Expected y=$y but got $v")
          assert(width == w, s"Expected width=$width but got $w")
          assert(height == h, s"Expected height=$height but got $h")
        case other =>
          fail(s"Expected Location containing Rectangle, got $other")

  // Tests from original project
  testBoundingBox("simple ellipse", simpleEllipse, -50, -30, 100, 60)
  testBoundingBox("simple rectangle", simpleRectangle, 0, 0, 80, 120)
  testBoundingBox("simple location", simpleLocation, 70, 30, 80, 120)
  testBoundingBox("basic group", basicGroup, -50, -30, 100, 70)
  testBoundingBox("simple group", simpleGroup, 150, 70, 350, 280)
  testBoundingBox("complex group", complexGroup, 30, 60, 470, 320)

end TestBoundingBox