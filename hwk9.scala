object Homework9 {
  def main(args: Array[String]) {
    val lst = List(5,4,11,2,3,1,0,9)
    println(merge_sort(lst))
    println(selection_sort(lst))
    println(insertion_sort(lst))
  }
  
  def merge_sort(xs: List[Int]): List[Int] = {
    val a = xs.length / 2
    if (a == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = 
        (xs, ys) match {
        case(Nil, ys) => ys
        case(xs, Nil) => xs
        case(x :: xs1, y :: ys1) =>
          if (x < y) x::merge(xs1, ys)
          else y::merge(xs, ys1)
      }
      val (left, right) = xs.splitAt(a)
      merge(merge_sort(left), merge_sort(right))
    }
    
  }
  def maximun(xs: List[Int]): List[Int] = 
    (List(xs.head) /: xs.tail) {
    (ys, x) =>
      if(x > ys.head) (x::ys)
      else (ys.head::x::ys.tail)
  }
  
  def selection_sort(xs: List[Int]): List[Int] = {
    def selection_sort_helper(xs: List[Int], accumulator: List[Int]): List[Int] = 
      if(xs.isEmpty) accumulator
      else {
        val ys = maximun(xs)
        selection_sort_helper(ys.tail, ys.head::accumulator)
      }
    selection_sort_helper(xs, Nil)
  }
    
  def insertion_sort(xs: List[Int]): List[Int] = {
    def insert(x: Int, xs: List[Int]): List[Int] = {
      if (xs.isEmpty || x <= xs.head) x :: xs
      else xs.head :: insert(x, xs.tail)
    }
    if (xs.isEmpty) Nil
    else insert(xs.head, insertion_sort(xs.tail))
  }
}