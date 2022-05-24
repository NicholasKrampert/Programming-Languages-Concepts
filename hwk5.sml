fun merge_sort _ nil = nil
  | merge_sort f [a] = [a]
  | merge_sort f L =
    let
      fun halve nil = ( nil , nil )
        | halve [a ] = ([ a], nil )
        | halve (a :: b :: rest ) =
          let
            val (x , y) = halve rest
          in
            (a :: x , b :: y )
          end
      fun merge ( nil , x) = x
        | merge (x , nil ) = x
        | merge (a ::b , x :: y) =
          if f(a, x) then a :: merge (b , x :: y)
          else x :: merge (a ::b , y );
      val (x , y) = halve L
    in
      merge ( merge_sort f x , merge_sort f y)
    end;

fun selection_sort _ nil = nil
  | selection_sort f [a ] = [a ]
  | selection_sort f (a :: b) =
    let
	  fun select s ([], out) = s :: (selection_sort f out)
		| select s (c :: d, out) =
		  if f(c, s) 
		    then select c (d, s :: out)
	      else select s (d, c :: out)
	in
	  select a (b, [])
	end;
	   
fun insertion_sort _ nil = nil
  | insertion_sort f (a :: b) =
    let
	  fun insert a (c :: d) =
	    if f(a, c)
		  then a :: c :: d
		else c :: (insert a d)
    in
	  insert a (insertion_sort f b)
	end;
