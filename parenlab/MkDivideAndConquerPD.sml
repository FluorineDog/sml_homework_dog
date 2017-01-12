functor MkDivideAndConquerPD(structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq

  (* Remove this line when you're done. *)
  (*exception NotYetImplemented*)

  exception NYI
  fun parenDist (parens : paren seq) : int option =
    let
      fun max a b = Int.max (a,b)
      fun max3 a b c = max (max a b) c
      fun dist S = case showt S of 
          EMPTY => ((~1,~1),~1,(~1,~1),~1)
        | ELT elt => if elt = OPAREN 
               then ((0,0),0,(1,1),0) else ((1,1),0,(0,0),0)
        | NODE (LSTR, RSTR) => 
          let
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
      case dist parens of (L, MS, R, MAX)
       => if(L=(0,0) andalso R=(0,0))then SOME (MAX-2) else NONE
    end
end