functor MkBigNumMultiply(structure BNA : BIGNUM_ADD
                         structure BNS : BIGNUM_SUBTRACT
                         sharing BNA.Util = BNS.Util) : BIGNUM_MULTIPLY =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  (* Remove this line when you're done. *)

  infix 6 ++ --
  infix 7 **

  fun at con index = 
    if index < (length con ) then nth con index
    else ZERO
  fun x ++ y = BNA.add (x, y)
  fun x -- y = BNS.sub (x, y)
  fun x ** y =


  val mul = op**
end
