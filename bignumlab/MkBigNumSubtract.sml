functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  (* Remove this line when you're done. *)
  infix 6 ++ --
  fun simplify res = 
    let
      val index_seq = mapIdx (fn (_,ZERO)=>~1|(n,ONE)=>n) res
      fun copy (a,0) = a
        | copy (_,b) = b
      val len = 1 + reduce copy ~1 index_seq 
    in     
      take (res, len)
    end
  fun x ++ y = BNA.add (x, y)
  fun x -- y =
    let
      val lx = length x 
      val ly = length y 
      val lm = Int.max (lx, ly)
      fun at con index = 
        if index < (length con ) then nth con index
        else ZERO
      val inv_y = tabulate (fn n=> if at y n = ZERO then ONE else ZERO) lm
      val overflow = (x ++ inv_y) ++ singleton ONE  
      val result = take (overflow,lm)
    in
      simplify result
    end
  val sub = op--
end
