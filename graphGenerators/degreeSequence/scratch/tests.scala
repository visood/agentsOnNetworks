components(al).sortBy{ case c => - numLinksOnCycle( projection(al)(c) )} match {
  case Nil => None
  case c1 :: cmps => ((Some(projection(al)(c1)): Option[AL]) /: cmps) 
    { case (None, _) => None;
      case (Some(al1), c) => Gkantsidis.connectTwo(al1, projection(al)(c))
    }
}


val al1: AL = Map(0 -> Vector(2, 6, 8, 1), 
                  5 -> Vector(6),
                  1 -> Vector(0),
                  6 -> Vector(5, 0, 4, 3),
                  9 -> Vector(3), 
                  2 -> Vector(0, 3, 8), 
                  3 -> Vector(2, 9, 6), 
                  8 -> Vector(0, 2), 
                  4 -> Vector(6)
                 )
val al2: AL = Map(14 -> Vector(15, 18), 
                  17 -> Vector(15, 18, 11), 
                  18 -> Vector(17, 14, 15), 
                  11 -> Vector(15, 17), 
                  15 -> Vector(11, 17, 14, 18)
                )
