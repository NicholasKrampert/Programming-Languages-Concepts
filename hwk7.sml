(* Nicholas Krampert Homework 7*)
datatype 'data tree =
  Empty 
  | Node of 'data tree * 'data * 'data tree;
  
fun isFull Empty = true
  | isFull (Node(a,b,c)) = 
    let
	  val x = isFull a
	  val y = isFull c
	in
	  if x = y then true
	  else false
	end;
	
fun makeBST [] a = Empty
  | makeBST (f::l) a = 
    let
	  val tree = Empty
	  fun insertHelp Empty f = Node (Empty, f, Empty)
	    | insertHelp (Node(left, tree, right)) f = 
		  if a(f, tree) then
		    Node(insertHelp left f, tree, right)
		  else
		    Node(left, tree, insertHelp right f)
	in
	  insertHelp (makeBST l a) f
	end;
	
fun searchBST (Node(l, d, r)) f e =
  if (e = d) then true
  else if f(e, d) then searchBST l f e
  else searchBST r f e;