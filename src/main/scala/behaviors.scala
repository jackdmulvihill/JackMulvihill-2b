package edu.luc.cs.laufer.cs371.shapes

import higherkindness.droste.*
import higherkindness.droste.data.Fix
import com.typesafe.scalalogging.LazyLogging

/**
  * Shape behaviors defined as ShapeF-algebras.
  * Each algebra is nonrecursive - recursion is handled by droste's cata.
  * Defines bounding box, size, depth, and scale behaviors over suitable carrier objects.
  */
object behaviors extends LazyLogging:

  import structures.ShapeF
  import ShapeF.*

  /** 
    * Bounding box algebra with precise result type.
    * Result type: Fix[ShapeF] representing Location(x, y, Rectangle(w, h))
    * 
    * The result is formally a Shape that is always a Location containing a Rectangle.
    * This is expressed precisely as: Fix[ShapeF] where the top-level is Location
    * containing another Fix[ShapeF] that is Rectangle.
    * 
    * Type structure: Fix(Location(x, y, Fix(Rectangle(w, h))))
    */
  val boundingBox: Algebra[ShapeF, Fix[ShapeF]] = Algebra:
    case Rectangle(w, h) =>
      logger.debug(s"Rectangle: bounding box at (0, 0) with dimensions $w x $h")
      Fix(ShapeF.Location(0, 0, Fix(ShapeF.Rectangle(w, h))))
    
    case Ellipse(sw, sh) =>
      // Ellipse is centered at origin, extends sw in each x direction, sh in each y direction
      val fullWidth = 2 * sw
      val fullHeight = 2 * sh
      val x = -sw
      val y = -sh
      logger.debug(s"Ellipse: bounding box at ($x, $y) with dimensions $fullWidth x $fullHeight")
      Fix(ShapeF.Location(x, y, Fix(ShapeF.Rectangle(fullWidth, fullHeight))))
    
    case Location(x, y, innerBox) =>
      // innerBox is Fix(Location(innerX, innerY, Fix(Rectangle(w, h))))
      // Extract the inner location and rectangle, then translate
      innerBox match
        case Fix(ShapeF.Location(innerX, innerY, rect)) =>
          val resultX = x + innerX
          val resultY = y + innerY
          logger.debug(s"Location($x, $y): translating box from ($innerX, $innerY) to ($resultX, $resultY)")
          Fix(ShapeF.Location(resultX, resultY, rect))
        case _ => 
          // Should not happen with proper algebra composition
          innerBox
    
    case Group(boxes) if boxes.isEmpty =>
      logger.debug("Empty group: bounding box at (0, 0) with dimensions 0 x 0")
      Fix(ShapeF.Location(0, 0, Fix(ShapeF.Rectangle(0, 0))))
    
    case Group(boxes) =>
      logger.debug(s"Group with ${boxes.length} shapes")
      // Extract all coordinates from Location(x, y, Rectangle(w, h))
      val coords = boxes.map:
        case Fix(ShapeF.Location(x, y, Fix(ShapeF.Rectangle(w, h)))) =>
          (x, y, w, h)
        case _ => 
          (0, 0, 0, 0) // Fallback
      
      val leftCoords = coords.map(_._1)
      val rightCoords = coords.map((x, _, w, _) => x + w)
      val topCoords = coords.map((_, y, _, _) => y)
      val bottomCoords = coords.map((_, y, _, h) => y + h)
      
      // Find extremes
      val minX = leftCoords.min
      val maxX = rightCoords.max
      val minY = topCoords.min
      val maxY = bottomCoords.max
      
      val boundingWidth = maxX - minX
      val boundingHeight = maxY - minY
      
      logger.debug(s"Group bounding box: ($minX, $minY) with dimensions $boundingWidth x $boundingHeight")
      Fix(ShapeF.Location(minX, minY, Fix(ShapeF.Rectangle(boundingWidth, boundingHeight))))

  /** 
    * Size algebra.
    * Counts the number of concrete leaf shapes (rectangles and ellipses).
    * Carrier object: Int
    */
  val size: Algebra[ShapeF, Int] = Algebra:
    case Rectangle(_, _) =>
      logger.debug("Rectangle leaf: size = 1")
      1
    
    case Ellipse(_, _) =>
      logger.debug("Ellipse leaf: size = 1")
      1
    
    case Location(_, _, innerSize) =>
      logger.debug(s"Location: size = $innerSize (passthrough)")
      innerSize
    
    case Group(sizes) =>
      val total = sizes.sum
      logger.debug(s"Group: size = $total (sum of ${sizes.length} children)")
      total

  /** 
    * Depth algebra.
    * Computes the height (depth) of the shape tree.
    * Carrier object: Int
    * Leaf shapes have depth 0.
    * Location and Group add 1 to the max depth of their children.
    */
  val depth: Algebra[ShapeF, Int] = Algebra:
    case Rectangle(_, _) =>
      logger.debug("Rectangle leaf: depth = 0")
      0
    
    case Ellipse(_, _) =>
      logger.debug("Ellipse leaf: depth = 0")
      0
    
    case Location(_, _, innerDepth) =>
      val result = 1 + innerDepth
      logger.debug(s"Location: depth = $result (1 + $innerDepth)")
      result
    
    case Group(depths) if depths.isEmpty =>
      logger.debug("Empty group: depth = 0")
      0
    
    case Group(depths) =>
      val maxChildDepth = depths.max
      val result = 1 + maxChildDepth
      logger.debug(s"Group: depth = $result (1 + max($maxChildDepth))")
      result

  /**
    * Scale algebra.
    * Scales a shape by a given factor.
    * Carrier object: Fix[ShapeF] (returns a new Shape)
    * This algebra transforms shapes by scaling dimensions and coordinates.
    * The recursion position A is already Fix[ShapeF], so we work with complete shapes.
    */
  def scale(factor: Double): Algebra[ShapeF, Fix[ShapeF]] = Algebra:
    case Rectangle(w, h) =>
      val scaledW = (w * factor).toInt
      val scaledH = (h * factor).toInt
      logger.debug(s"Scaling Rectangle: $w x $h -> $scaledW x $scaledH")
      Fix(ShapeF.Rectangle(scaledW, scaledH))
    
    case Ellipse(sw, sh) =>
      val scaledSW = (sw * factor).toInt
      val scaledSH = (sh * factor).toInt
      logger.debug(s"Scaling Ellipse: $sw x $sh -> $scaledSW x $scaledSH")
      Fix(ShapeF.Ellipse(scaledSW, scaledSH))
    
    case Location(x, y, scaledShape) =>
      val scaledX = (x * factor).toInt
      val scaledY = (y * factor).toInt
      logger.debug(s"Scaling Location: ($x, $y) -> ($scaledX, $scaledY)")
      Fix(ShapeF.Location(scaledX, scaledY, scaledShape))
    
    case Group(scaledShapes) =>
      logger.debug(s"Scaling Group with ${scaledShapes.length} shapes")
      Fix(ShapeF.Group(scaledShapes))

end behaviors