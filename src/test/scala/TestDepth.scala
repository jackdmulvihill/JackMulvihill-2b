package edu.luc.cs.laufer.cs371.shapes

import org.scalatest.funsuite.AnyFunSuite
import higherkindness.droste.scheme

import TestFixtures.*
import structures.*
import structures.shape.*
import behaviors.*

/**
  * Apply depth algebra using cata function.
  * Test suite adapted from the original project.
  */
class TestDepth extends AnyFunSuite:

  def testDepth(description: String, s: Shape, expected: Int): Unit =
    test(description):
      // Apply depth algebra using scheme.cata
      val result = scheme.cata(depth).apply(s)
      assert(expected == result, s"Expected depth $expected but got $result")

  // Tests from original project 
  testDepth("simple ellipse", simpleEllipse, 0)
  testDepth("simple rectangle", simpleRectangle, 0)
  
  // Test Location wrapper (adds 1 to depth)
  testDepth("simple location", simpleLocation, 1)
  
  // Test Groups
  testDepth("basic group", basicGroup, 1)
  testDepth("simple group", simpleGroup, 2)
  testDepth("complex group", complexGroup, 5)
  
  // Additional edge cases
  testDepth("empty group", group(), 0)
  
  testDepth("nested locations", 
    location(10, 20, location(30, 40, rectangle(50, 60))), 
    2)
  
  testDepth("group with only leaves", 
    group(rectangle(10, 20), ellipse(30, 40), rectangle(50, 60)), 
    1)
  
  testDepth("group with one location", 
    group(
      rectangle(10, 20),
      location(30, 40, ellipse(50, 60))
    ), 
    2)
  
  testDepth("deeply nested structure",
    location(0, 0,
      group(
        location(10, 10,
          group(
            location(20, 20, 
              rectangle(30, 30)
            )
          )
        )
      )
    ), 
    5)
  
  testDepth("group with mixed nesting depths",
    group(
      rectangle(10, 20),
      location(30, 40, ellipse(50, 60)),
      location(70, 80, 
        location(90, 100, rectangle(110, 120))
      )
    ),
    3)

end TestDepth