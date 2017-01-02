functor MkDivideAndConquerPD(structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq

  (* Remove this line when you're done. *)
  (*exception NotYetImplemented*)

  fun parenDist (parens : paren seq) : int option =
    let
      fun max a b = if(a<b)then b else a
      fun max3 a b c = max c (max a b)

      fun dist (_, 0) = (~1, ~1, ~1, ~1)
        | dist (beg, 1) = if(nth parens beg = OPAREN) then (0,0,1,0) else (1,0,0,0)
        | dist (beg:int, size:int):int*int*int*int = 
        let
          val mid = beg + size div 2;
          val (leftL, midL, rightL, maxL) = dist(beg, size div 2)
          val (leftR, midR, rightR, maxR) = dist(mid, size+beg - mid) 
          val result = if(rightL = leftR) 
            then (leftL, midL + rightL + leftR + midR, rightR, (max3 maxL (rightL+leftR) maxR))
            else if (rightL > leftR) then (leftL, midL, rightL+leftR+midR+rightR, maxL) 
            else (leftL+midL+rightL+leftR, midR , rightR, maxR)
        in
          result
        end
    in 
      let 
        val (left, mid, right, max) = dist(0, length(parens))
      in
        if(left = 0 andalso right = 0) then (SOME (max-2)) else NONE
      end
    end
end