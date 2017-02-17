fun q(a : int, b : int) =
  a*a + 2*a*b + b*b

fun sort(a : int, b: int) =
  if a > b
  then (a, b)
  else (b, a)

fun sort3(a: int, b : int, c: int) =
val sort1 = sort(a, b)
		(#1 sort(#1 sort1, c), sort(#2 sort(#1 sort1, c), #2 sort1)
	    
    


