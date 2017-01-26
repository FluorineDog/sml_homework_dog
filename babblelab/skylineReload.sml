fun f _ = 0
fun extrema (s:int seq): int option seq = 
  let 
    fun cmp f (i,j) =
      (j < 0) orelse (j > length s - 1)	(* first/last true *) 
      orelse f (nth s i, nth s j)
    fun less = (fn(a,b)=>a<b)
    fun greater = (fn(a,b)=>a>b)
    fun check n = if 
      (cmp less (n-1, n) andalso cmp more (n, n+1))
      orelse
      (cmp more (n-1, n) andalso cmp less (n, n+1))
      then SOME (nth s n)
      else NONE
  in 
    tabulate check (length s)
  end 