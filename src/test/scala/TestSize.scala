package edu.luc.cs.laufer.cs371.shapes

import org.scalatest.funsuite.AnyFunSuite
import higherkindness.droste.scheme

import TestFixtures.*
import structures.*
import structures.shape.*
import behaviors.*

/**
  * Apply size algebra using cata function.
  * Test suite adapted from the original project.
  */
class TestSize extends AnyFunSuite:

  def testSize(description: String, s: Shape, expected: Int): Unit =
    test(description):
      // Apply size algebra using scheme.cata
      val result = scheme.cata(size).apply(s)
      assert(expected == result, s"Expected size $expected but got $result")

  // Tests from original project
  testSize("simple ellipse", simpleEllipse, 1)
  testSize("simple rectangle", simpleRectangle, 1)
  
  // Test Location wrapper (doesn't add to count, just wraps)
  testSize("simple location", simpleLocation, 1)
  
  // Test Groups
  testSize("basic group", basicGroup, 2)
  testSize("simple group", simpleGroup, 2)
  testSize("complex group", complexGroup, 5)
  
  // Additional edge cases
  testSize("empty group", group(), 0)
  
  testSize("nested locations", 
    location(10, 20, location(30, 40, rectangle(50, 60))), 
    1)
  
  testSize("group with only rectangles", 
    group(rectangle(10, 20), rectangle(30, 40), rectangle(50, 60)), 
    3)
  
  testSize("group with only ellipses", 
    group(ellipse(10, 20), ellipse(30, 40), ellipse(50, 60)), 
    3)
  
  testSize("group with mixed leaf shapes", 
    group(rectangle(10, 20), ellipse(30, 40), rectangle(50, 60)), 
    3)
  
  testSize("group with locations wrapping leaves",
    group(
      location(10, 20, rectangle(30, 40)),
      location(50, 60, ellipse(70, 80)),
      location(90, 100, rectangle(110, 120))
    ), 
    3)
  
  testSize("nested groups",
    group(
      rectangle(10, 20),
      group(ellipse(30, 40), rectangle(50, 60)),
      ellipse(70, 80)
    ), 
    4)
  
  testSize("deeply nested with locations and groups",
    location(0, 0,
      group(
        location(10, 10,
          group(
            rectangle(20, 20),
            location(30, 30, ellipse(40, 40))
          )
        ),
        rectangle(50, 50)
      )
    ), 
    3)
  
  testSize("large group",
    group(
      rectangle(10, 20),
      ellipse(30, 40),
      location(50, 60, rectangle(70, 80)),
      group(
        ellipse(90, 100),
        rectangle(110, 120)
      ),
      location(130, 140, 
        group(
          rectangle(150, 160),
          ellipse(170, 180),
          rectangle(190, 200)
        )
      )
    ),
    8)
  
  testSize("multiple nesting levels with same total",
    group(
      group(rectangle(10, 20), ellipse(30, 40)),
      group(rectangle(50, 60), ellipse(70, 80))
    ),
    4)

end TestSize