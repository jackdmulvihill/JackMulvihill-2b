package edu.luc.cs.laufer.cs371.shapes

import higherkindness.droste.scheme
import higherkindness.droste.data.Fix

import structures.*
import structures.shape.*
import structures.ShapeF
import behaviors.*
import TestFixtures.*

@main def runShapesDemo(): Unit =
  println("=== Shapes F-Algebra Demo ===\n")
  
  println("Simple Location:")
  println(s"  Shape: $simpleLocation")
  
  val bbox = scheme.cata(boundingBox).apply(simpleLocation)
  bbox match
    case Fix(ShapeF.Location(x, y, Fix(ShapeF.Rectangle(w, h)))) =>
      println(s"  Bounding Box: x=$x, y=$y, width=$w, height=$h")
    case _ =>
      println("  Unexpected bounding box format")
  
  val shapeSize = scheme.cata(size).apply(simpleLocation)
  println(s"  Size: $shapeSize")
  
  val shapeDepth = scheme.cata(depth).apply(simpleLocation)
  println(s"  Depth: $shapeDepth")
  
  val scaled = scheme.cata(scale(2.0)).apply(simpleLocation)
  println(s"  Scaled 2x: $scaled")
  
  println("\nComplex Group:")
  println(s"  Shape: $complexGroup")
  
  val complexBbox = scheme.cata(boundingBox).apply(complexGroup)
  complexBbox match
    case Fix(ShapeF.Location(x, y, Fix(ShapeF.Rectangle(w, h)))) =>
      println(s"  Bounding Box: x=$x, y=$y, width=$w, height=$h")
    case _ =>
      println("  Unexpected bounding box format")
  
  val complexSize = scheme.cata(size).apply(complexGroup)
  println(s"  Size: $complexSize")
  
  val complexDepth = scheme.cata(depth).apply(complexGroup)
  println(s"  Depth: $complexDepth")
  
  println("\n=== Demo Complete ===")