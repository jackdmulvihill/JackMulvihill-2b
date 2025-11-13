package edu.luc.cs.laufer.cs371.shapes

import structures.*
import structures.shape.*

/**
  * Test fixtures isomorphic to those in the original project.
  * Sample shape trees created using factory functions.
  */
object TestFixtures:

  val simpleEllipse = ellipse(50, 30)

  val simpleRectangle = rectangle(80, 120)

  val simpleLocation = location(70, 30, rectangle(80, 120))

  val basicGroup = group(ellipse(50, 30), rectangle(20, 40))
  
  val simpleGroup = group(
    location(200, 100, ellipse(50, 30)),
    location(400, 300, rectangle(100, 50))
  )

  val complexGroup =
    location(50, 100,
      group(
        ellipse(20, 40),
        location(150, 50,
          group(
            rectangle(50, 30),
            rectangle(300, 60),
            location(100, 200,
              ellipse(50, 30)
            )
          )
        ),
        rectangle(100, 200)
      )
    )

end TestFixtures