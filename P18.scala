class P18 extends Euler {

  val triangle: IndexedSeq[IndexedSeq[Int]] = Vector(
    Vector(75),
    Vector(95,64),
    Vector(17,47,82),
    Vector(18,35,87,10),
    Vector(20, 4,82,47,65),
    Vector(19, 1,23,75, 3,34),
    Vector(88, 2,77,73, 7,63,67),
    Vector(99,65, 4,28, 6,16,70,92),
    Vector(41,41,26,56,83,40,80,70,33),
    Vector(41,48,72,33,47,32,37,16,94,29),
    Vector(53,71,44,65,25,43,91,52,97,51,14),
    Vector(70,11,33,28,77,73,17,78,39,68,17,57),
    Vector(91,71,52,38,17,14,91,43,58,50,27,29,48),
    Vector(63,66, 4,68,89,53,67,30,73,16,69,87,40,31),
    Vector( 4,62,98,27,23, 9,70,98,73,93,38,53,60, 4,23))

  type Pos = (Int, Int)

  def isInTriangle(p: Pos) = p match {
    case (r, c) if r < triangle.size && c < r+1 => true
    case _ => false 
  }


  def choices(pos: Pos): List[Pos] = {
    val (row, col) = pos
    List((row+1, col), (row+1, col+1)) filter isInTriangle
  }

  def value(pos: Pos) = {
    val (row, col) = pos
    triangle(row)(col)
  }

  object brute {

    def from(pos: Pos): List[List[Int]] = {
      if (isInTriangle(pos))
        choices(pos) match {
          case List() => List(List(value(pos)))
          case choices => for {
            choice <- choices
            path <- from(choice)
          } yield value(pos) :: path
        }
      else
        List()
    }

    def soln = from((0,0)) map (l => l reduce (_+_)) max

  }

  val memo = new Memo[Pos, Int]

  def best(pos: Pos):Int = memo(pos) {
    value(pos) + ( 
      choices(pos) map best reduceOption math.max getOrElse 0)
  }


  def soln = best((0,0))
}


object P18 extends P18

// vim: set ts=2 sw=2 et:
