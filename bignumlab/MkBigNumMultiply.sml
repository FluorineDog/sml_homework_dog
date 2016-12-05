functor MkBigNumMultiply(structure BNA : BIGNUM_ADD
                         structure BNS : BIGNUM_SUBTRACT
                         sharing BNA.Util = BNS.Util) : BIGNUM_MULTIPLY =
struct
  structure Util = BNA.Util
  open Util
  open Seq
  open Primitives
  (* Remove this line when you're done. *)

  infix 6 ++ --
  infix 7 **
  fun simplify res = 
    let
      val index_seq = mapIdx (fn (_,ZERO)=>(~1)|(n,ONE)=>n) res
      fun copy (a,~1) = a
        | copy (_,b) = b
      val len = 1 + reduce copy ~1 index_seq 
    in     
      take (res, len)
    end
  fun at con index = 
    if index < (length con ) then nth con index
    else ZERO
  fun x ++ y = BNA.add (x, y)
  fun x -- y = BNS.sub (x, y)
  fun multiply x y =
    case (length x, length y) 
      of  (0,_) => empty()
        | (1,_) => if nth x 0 = ZERO then empty() else y 
        | (lx, ly) => if (lx > ly) then multiply y x else  
        let
          val mid = lx div 2
          val (left_x, right_x) = (subseq x (0, mid), (subseq x (mid, lx-mid)))
          val (left_y, right_y) = (subseq y (0, mid), (subseq y (mid, ly-mid)))
          val (x1y1, x2y2, xxyy) = par3 (
            fn _ => multiply left_x left_y,
            fn _ => multiply right_x right_y,
            fn _ => multiply (left_x++right_x) (left_y++right_y)
          )
          val cross = xxyy--x1y1--x2y2
          fun sci_power real power =
            let val zeros = tabulate (fn _ =>ZERO) power
            in append (zeros,real) 
            end
        in
          (sci_power x2y2 (2*mid))++(sci_power cross mid)++x1y1
        end

  fun x ** y = multiply x y

  val mul = op**
end