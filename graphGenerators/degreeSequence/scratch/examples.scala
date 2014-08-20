// a scratch file, examples to see what code to write

//to merge two adjacency lists
val al1 = Map(1 -> IndexedSeq(2,3), 2 -> IndexedSeq(1, 3), 3 -> IndexedSeq(1,2,4), 4 -> IndexedSeq(3))
val al2 = Map(3 -> IndexedSeq(5, 6), 4 -> IndexedSeq(5), 5 -> IndexedSeq(3, 4), 6 -> IndexedSeq(3))
val grpd12 = (al1.toSeq ++ al2.toSeq).groupBy(_._1)
val al12 = grpd12.mapValues(_.flatMap(_._2).toIndexedSeq)


//links, adj-list confusion!!!
val es1 =  IndexedSeq( (1,2), (1,3), (2,3), (3, 4))
val es2 = IndexedSeq((3, 5), (3, 6), (4, 5))
val es1fb = es1 ++ es1.map{ case (x, y) => (y, x)}
val es2fb = es2 ++ es2.map{ case (x, y) => (y, x)}
val al1 = es1fb.groupBy(_._1).mapValues(_.map(_._2).sorted)
val al2 = es2fb.groupBy(_._1).mapValues(_.map(_._2).sorted)
