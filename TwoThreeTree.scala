package hwk12
 

trait Tree[X <: Ordered[X]] {
  def contains(e: X): Boolean 
  def insert(e: X) = ins(e) match {
    case FourNode(t1, a, t2, b, t3, c, t4) => TwoNode(TwoNode(t1, a, t2), b, TwoNode(t3, c, t4))
    case n => n 
  }
  def ins(e: X): Tree[X] 
  def height: Int
}

case class Leaf[X <: Ordered[X]]() extends Tree[X] {
  // TODO
  def contains(e: X): Boolean = false
  def insert(e: X): ins(e) match {
    case FourNode(t1, a, t2, b, t3, c, t4) => TwoNode(TwoNode(t1, a, t2), b, TwoNode(t3, c, t4))
	case n => n
  }
  def ins(e: X): Tree[X] = Tree[e] 
  def height = None
}

case class TwoNode[X <: Ordered[X]](left: Tree[X], x:X, right: Tree[X]) extends Tree[X] {
  // TODO
  def promote(e: MaybePseudo,
              f: TwoNode[X] => MaybePseudo,
              g: ThreeNode[X] => ThreeNode[X]): MaybePseudo =
              e.fold(f, t => Right(g(t)))
  def contains(e: X): Boolean = {
    if(e < x) left.contains(e)
	else e == x || right.contains(e)
  }
  def insert(e: X) = {
    if(e < x) promote(left.insert(e), 
	                  p => Right(ThreeNode(p.left, p.x, p.right, x, right))
					  TwoNode(_, x, right))
	else promote(right.insert(e),
	             p => Right(ThreeNode(left, x, p.left, p.x, p.right))
				 TwoNode(left, x, _))
  }
  def ins(e: X): Tree[X] = {
    if(e < x) left.insert(e)
	else if (e > x) right.insert(e)
	else insert(e)
  }
  def height = 1 + left.height
}

case class ThreeNode[X <: Ordered[X]](left: Tree[X], x1:X, middle:Tree[X], x2:X, right: Tree[X]) extends Tree[X] {
  // TODO
  def promote(e: MaybePseudo,
              f: TwoNode[X] => MaybePseudo,
              g: ThreeNode[X] => ThreeNode[X]): MaybePseudo =
              e.fold(f, t => Right(g(t)))
  def contains(e: X): Boolean = {
    if(e < x1) left.contains(e)
	else if(e > x1 && e < x2) middle.contains(e)
	else if(e > x2) right.contains(e)
	else e == x
  }
  def insert(e: X) = {
    if(e < x1) promote(left.insert(e),
	                   p => left(TwoNode(p, x1, TwoNode(middle, x2, right))),
					   ThreeNode(_, x1, middle, x2, right))
	else if(e < x2) promote(middle.insert(e), 
	                        p => left(TwoNode(TwoNode(left, x1, p.left), p.x, TwoNode(p.r, x2, right))),
							ThreeNode(left, x1, _, x2, right))
	else promote(right.insert(e), 
	             p => left(TwoNode(TwoNode(left, x1, middle), x2, p)),
				 ThreeNode(left, x1, middle, x2, _))
  }
  def ins(e: X) = {
    if(e < x1) left.insert(e)
	else if(e < x2) middle.insert(e)
	else if(e > x2) right.insert(e)
	else insert(e)
  }
  def height = 1 + left.height
}

case class FourNode[X<:Ordered[X]](t1:Tree[X], a:X, t2:Tree[X], b:X, t3:Tree[X], c:X, t4:Tree[X]) extends Tree[X] {
  def contains(e: X): Boolean = throw new Exception()
  def ins(e: X): Tree[X] = throw new Exception()
  def height: Int = throw new Exception()
}
 
case class Num(i: Int) extends Ordered[Num] {
  def compare(that: Num) = i - that.i
  override def toString = i.toString
}
case class Alpha(c: Char) extends Ordered[Alpha] {
  def compare(that: Alpha) = c - that.c
  override def toString = c.toString
}

object TwoThreeTree {
    def main(arg: Array[String]) {
	  val input = List(3, 4, 2, 10, 9, 1, 5, 6, 11, 12, 13, 14, 15).map(i=>Num(i))
	  
	  val t = input.foldLeft[Tree[Num]](Leaf())((l, i) => { val x = l.insert(i); println(x.height + " " + x); x }) 
	  
	  println(t.contains(Num(5)))
	  println(t.contains(Num(14)))
	  println(t.contains(Num(7))) 
	  println(t.contains(Num(17))) 
	  
	  val input2 = List('a', 'c', 'd', 'g', 'e', 'z', 'r', 'k', 'l', 'p', 'y').map(i => Alpha(i))
	  
	  val t2 = input2.foldLeft[Tree[Alpha]](Leaf())((l, i) => { val x = l.insert(i); println(x.height + " " + x); x }) 
	  println(t2.contains(Alpha('d')))
	  println(t2.contains(Alpha('z')))
	  println(t2.contains(Alpha('m'))) 
	  println(t2.contains(Alpha('s'))) 
  }
}
