functor MkSkyline(structure S : SEQUENCE) : SKYLINE =
struct
  structure Seq = S
  open Primitives
  open Seq

  (* Remove this line when you're done. *)
  (*exception NotYetImplemented*)
  datatype side_t = LEFT | RIGHT 

  fun natural_cmp (l,r) = if l < r then LESS else if l > r then GREATER else EQUAL 
  fun combine (leftHeights, rightHeights) =
    let 
      (*attach side_t infomation*)
      val leftAttHei = map (fn (i, h)=> (i, h, LEFT)) leftHeights 
      val rightAttHei = map (fn (i, h)=> (i, h, RIGHT)) rightHeights 
      (*combine them together*)
      val preload = merge (fn ((i,_,_), (j,_,_)) => natural_cmp(i, j)) leftAttHei rightAttHei
      (*copy to make building of both side visable at each point*)
      fun copy side (left_bui, right_bui) =
        case right_bui of (ri, _, rs)=> 
          if rs=side then right_bui
          else case left_bui of (_, lh, ls) => (ri, lh, ls)
      val left_copy = scani (copy LEFT) (0, 0, LEFT) preload
      val right_copy = scani (copy RIGHT) (0, 0, RIGHT) preload
      (*pick out the higher building*)
      val res = map2 (fn((li, lh, ls), (ri, rh, rs)) => if lh > rh then (li, lh) else (ri, rh))
        left_copy right_copy
    in
      filterIdx (fn(i, x) => i=0 orelse #2 (nth res (i-1)) <> #2 x) res
    end 

  (*fun skyline (buildings : (int *int * int) seq) : (int * int) seq =
    let
      val tmp = map (fn (beg, height, e) => fromList [(beg, height), (e, 0)]) buildings
      (*this is the so-called "divide and conquer" step *)
      val result = reduce combine (empty()) tmp
    in
      result
    end
  *)
  fun skyline (buildings : (int * int * int) seq) : (int * int) seq =
    case length buildings of 0 => empty()
    | 1 => (case nth buildings 0 of (beg, height, e) => fromList [(beg, height), (e, 0)])
    | n => 
      let
        val mid = n div 2
        val (left, right) = par(
          fn _=>skyline(subseq buildings (0,mid)) , 
          fn _=>skyline(subseq buildings (mid,n-mid))
        )
        (*val left = skyline(subseq buildings (0,mid))
        val right = skyline(subseq buildings (mid,n-mid))*)
        (*this is the so-called "divide and conquer" step *)
      in 
        combine (left,right)
      end
end
