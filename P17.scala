object P17 extends Euler {

  abstract sealed class Place
  case class TenOne(m:Int, n:Int)
  case class One(n:Int)
  case class Ten(n:Int)
  case class Hundred(n:Int)
  case class Thousand(n:Int)

  def inWords(n: Int) = {
    val (o :: t :: rest) = n.toString.map(_.toInt - 48).reverse.toList match {
      case List() => 0 :: 0 :: List()
      case List(n) => n :: 0 :: List()
      case List(m,n) => m :: n :: List()
      case l => l
    }
    val strs = (TenOne(t,o) :: 
      (rest.zip(Seq(Hundred, Thousand)).map { case (n,p) => p(n) })).
    reverse.map {
      case Thousand(n) => "%s thousand".format(simple(n))
      case Hundred(0) => ""
      case Hundred(n) => "%s hundred".format(simple(n))
      case TenOne(0, 0) => ""
      case TenOne(m, n) if m < 2 => (m,n) match {
        case (0,n) => simple(n)
        case (1,0) => "ten"
        case (1,1) => "eleven"
        case (1,2) => "twelve"
        case (1,3) => "thirteen"
        case (1,4) => "fourteen"
        case (1,5) => "fifteen"
        case (1,8) => "eighteen"
        case (1,n) if n > 5 => "%steen" format simple(n)
      }
      case TenOne(m,n) => m match {
        case 2 => "twenty %s" format simple(n)
        case 3 => "thirty %s" format simple(n)
        case 4 => "forty %s" format simple(n)
        case 5 => "fifty %s" format simple(n)
        case 8 => "eighty %s" format simple(n)
        case 6|7|9 => "%sty %s".format(simple(m), simple(n))
      }
    }
    
    (if (!rest.isEmpty && (o > 0 || t > 0)) 
      (strs.last :: "and" :: strs.reverse.tail) reverse
     else 
       strs
     ).reduce(_+" "+_)
  }


  def simple(n:Int) = Seq("",
    "one", "two", "three", "four", "five", 
    "six", "seven", "eight", "nine")(n)

  def soln = 
    (1 to 1000) map (inWords) map (_.replace(" ","")) reduce (_+_) size

}

// vim: set ts=2 sw=2 et:
