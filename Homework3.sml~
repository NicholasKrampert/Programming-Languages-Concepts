fun gcd (a, 0) = a
  | gcd (a, b) = gcd (b, a - b*(a div b));

fun simplify(a, b) =
  let
    cd = gcd(a, b)
  in
    a div cd
    b div cd
  end;

fun add((a, b), (c, d)) =
  let
    val tot = 0
  in
    if b = d then tot = a + c :: b
    else 
      let
        val sa = simplify(a, b)
        val sc = simplify(c, d)
      in
        if sa = sc then tot = a + c :: sa
        else (a * sc + c * sa) :: (sa * sc)
      end;
  end;

fun times((a, b), (c, d)) =
  let
    val top = a * c
    val bot = b * d
  in
    simplify(top, bot)
  end;

fun addAll(valList) =
  if null valList then (o, 1)
  else
    let
      val tuple = hd valList
    in
      add(tuple, addAll(valList))
    end;

fun timesAll(valList) = 
  if null valList then (1, 1)
  else
    let
      val tuple = hd valList
    in
      times(tuple, timesAll(valList))
    end;

fun lessThan((a, b), (c, d)) = 
  if b = d 
    then if a < c 
      then true
    else
      false
  else if b < d
    then if a < c
       then false
    else
      true
  else
    if a > c
      then true
    else
      false

fun insert((a, b), valList) = 
  if null valList 
    then [(a, b)]
  else if lessThan((a, b), hd valList) 
    then (a, b) :: valList
  else hd valList :: insert((a, b), tl valList)

fun sort(valList) = 
  let
    val newList = null
  in
    if null valList
      then []
    else 
      while valList !(null)
        insert(hd valList, newList)
  end;
 
