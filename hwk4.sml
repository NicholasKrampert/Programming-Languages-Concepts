(* Homework 4 Nicholas Krampert*)
fun zip (nil, _) = nil
  | zip (_, nil) = nil
  | zip (a :: b, x :: y) = (a, x) :: zip(b, y);

fun unzip [] = ([], [])
  | unzip ((a, b) :: tail) = 
    let
      val (c, d) = unzip tail
    in
      (a :: c, b :: d)
    end;
    
fun zip3 (nil, _, _) = nil
  | zip3 (_, nil, _) = nil
  | zip3 (_, _, nil) = nil
  | zip3 (a :: b, c :: d, e :: f) = (a, c, e) :: zip3(b, d, f);
  
fun unzip3 [] = ([], [], [])
  | unzip3 ((a, b, c) :: tail) = 
    let
      val (d, e, f) = unzip3 tail
    in
      (a :: d, b :: e, c :: f)
    end;

fun zipWithIndex (a :: b) =
  let
    fun tupleHelper (a :: b, num) = 
      let
        val tuple = (num, a)
      in
        tuple :: tupleHelper(b, num + 1)
      end;
  in
    (0, a) :: tupleHelper(b, 1)
  end;

fun flatten [] = []
  | flatten (a :: b) = a @ (flatten b);

fun flatten2 [] = []
  | flatten2 ((a, b) :: c) =[a,b] @ (flatten2 c);


