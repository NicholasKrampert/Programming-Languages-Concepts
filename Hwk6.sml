fun reduceR f [a] = a
  | reduceR f (a :: b) = f (a, reduceR f b);

fun zip (nil, _) = nil
  | zip (_, nil) = nil
  | zip (a :: b, x :: y) = (a, x) :: zip(b, y);

fun vectorAdd (a :: b, x :: y) = 
  let
    val l = zip(a::b, x::y)
  in
    map (fn (c,d) => c + d) l
  end;
  
fun svProduct m l = map (fn(a) => a * m) l;

fun vmProduct a x =
  let
    val z = zip(a,x)
	val something = map (fn(a,x) => svProduct a x) z
  in
    reduceR vectorAdd something
  end;
  
fun matrixProduct a x = 
  let
    val l = map (fn(b, x) => vmProduct (b x)) a 
  in
    l
  end;