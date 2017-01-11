functor MkDivideAndConquerPD(structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq

  (* Remove this line when you're done. *)
  (*exception NotYetImplemented*)

  (*fun parenDist (parens : paren seq) : int option =*)
    (*let
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
    end*)
  exception NYI
  fun parenDist (parens : paren seq) : int option =
    let
      fun max a b = Int.max (a,b)
      fun max3 a b c = max (max a b) c
      fun dist S = case length S of 
          0 => ((~1,~1),~1,(~1,~1),~1)
        | 1 => if nth S 0 = OPAREN then ((0,0),0,(1,1),0) else ((1,1),0,(0,0),0)
        | n => 
          let
            val mid = n div 2
            val (LSTR, RSTR) = (take (S,mid), drop (S,mid))
            val (L1, MS1, (RP1, RS1), MAX1) = dist LSTR
            val ((LP2, LS2), MS2, R2, MAX2) = dist RSTR
            val result = case Int.compare (RP1,LP2) of 
                EQUAL  =>(L1, MS1+RS1+LS2+MS2, R2, max3 MAX1 MAX2 (LS2+RS1))
              | LESS   =>let
                         val (LP1, LS1) = L1
                         val LP = LP2-RP1+LP1
                         val LS = LS2+RS1+MS1+LS1
                         in ((LP,LS), MS2, R2, MAX2)end 
              | GREATER=>let 
                          val (RP2, RS2)=R2
                          val RP = RP1-LP2+RP2
                          val RS = RS1+LS2+MS2+RS2 
                        in (L1, MS1,(RP,RS), MAX1) end
          in
            result
          end

          (*((LP,LS),MS,(RP,RS),MAX)*)
    in
      case dist parens of (L, MS, R, MAX) => if(L=(0,0) andalso R=(0,0))then SOME (MAX-2) else NONE
    end
end