package edu.luc.cs.laufer.cs371.shapes

import org.scalatest.funsuite.AnyFunSuite
import higherkindness.droste.scheme

import TestFixtures.*
import structures.*
import structures.shape.*
import behaviors.*

/**
  * Apply scale algebra using cata function.
  * Test suite adapted from the original project.
  */
class TestScale extends AnyFunSuite:

  def testScale(description: String, s: Shape, factor: Double, expected: Shape): Unit =
    test(description):
      // Apply scale algebra using scheme.cata
      val result = scheme.cata(scale(factor)).apply(s)
      assert(expected == result, s"Expected $expected but got $result")

  // Tests from original project
  testScale("simple ellipse scale 2.0", 
    simpleEllipse, 
    2.0, 
    ellipse(100, 60))
  
  testScale("simple ellipse scale 0.5", 
    simpleEllipse, 
    0.5, 
    ellipse(25, 15))
  
  testScale("simple rectangle scale 2.0", 
    simpleRectangle, 
    2.0, 
    rectangle(160, 240))
  
  testScale("simple rectangle scale 0.5", 
    simpleRectangle, 
    0.5, 
    rectangle(40, 60))
  
  // Test Location scaling (scales both position and shape)
  testScale("simple location scale 2.0", 
    simpleLocation, 
    2.0, 
    location(140, 60, rectangle(160, 240)))
  
  testScale("simple location scale 0.5", 
    simpleLocation, 
    0.5, 
    location(35, 15, rectangle(40, 60)))
  
  // Test Group scaling
  testScale("basic group scale 2.0", 
    basicGroup, 
    2.0, 
    group(ellipse(100, 60), rectangle(40, 80)))
  
  testScale("basic group scale 0.5", 
    basicGroup, 
    0.5, 
    group(ellipse(25, 15), rectangle(10, 20)))
  
  testScale("simple group scale 2.0", 
    simpleGroup, 
    2.0, 
    group(
      location(400, 200, ellipse(100, 60)),
      location(800, 600, rectangle(200, 100))
    ))
  
  testScale("simple group scale 0.5", 
    simpleGroup, 
    0.5, 
    group(
      location(100, 50, ellipse(25, 15)),
      location(200, 150, rectangle(50, 25))
    ))
  
  testScale("complex group scale 2.0", 
    complexGroup, 
    2.0,
    location(100, 200,
      group(
        ellipse(40, 80),
        location(300, 100,
          group(
            rectangle(100, 60),
            rectangle(600, 120),
            location(200, 400,
              ellipse(100, 60)
            )
          )
        ),
        rectangle(200, 400)
      )
    ))
  
  // Additional test cases
  testScale("identity scale 1.0 rectangle", 
    rectangle(100, 200), 
    1.0, 
    rectangle(100, 200))
  
  testScale("identity scale 1.0 ellipse", 
    ellipse(50, 30), 
    1.0, 
    ellipse(50, 30))
  
  testScale("empty group scale 2.0", 
    group(), 
    2.0, 
    group())
  
  testScale("nested locations scale 3.0", 
    location(10, 20, location(30, 40, rectangle(50, 60))), 
    3.0,
    location(30, 60, location(90, 120, rectangle(150, 180))))
  
  testScale("group with mixed shapes scale 0.25",
    group(
      rectangle(20, 40),
      ellipse(60, 80),
      location(100, 200, rectangle(10, 20))
    ),
    0.25,
    group(
      rectangle(5, 10),
      ellipse(15, 20),
      location(25, 50, rectangle(2, 5))
    ))
  
  testScale("nested groups scale 1.5",
    group(
      rectangle(10, 20),
      group(ellipse(20, 30), rectangle(40, 50))
    ),
    1.5,
    group(
      rectangle(15, 30),
      group(ellipse(30, 45), rectangle(60, 75))
    ))
  
  testScale("scale zero produces zero-sized shapes",
    rectangle(100, 200),
    0.0,
    rectangle(0, 0))
  
  testScale("scale large factor",
    ellipse(5, 10),
    10.0,
    ellipse(50, 100))
  
  testScale("scale with decimal factor",
    rectangle(100, 100),
    0.75,
    rectangle(75, 75))
  
  testScale("scale with negative coordinates",
    location(-50, -100, rectangle(20, 40)),
    2.0,
    location(-100, -200, rectangle(40, 80)))

end TestScale