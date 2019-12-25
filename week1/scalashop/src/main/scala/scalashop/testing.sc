import scala.math
val w = 32
val numTasks = 3
val gap = (1.0 * w / numTasks).ceil.asInstanceOf[Int]

val start = (0 until w).by(gap)
val end1 = start.map(x => if (w < x + gap) w else x + gap)
val end = (gap to w).by(gap)

val points = start.zip(end)
val points2 = start.zip(end1)


start.tail
