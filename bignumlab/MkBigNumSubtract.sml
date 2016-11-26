functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  (* Remove this line when you're done. *)
  exception NotYetImplemented

  infix 6 ++ --

  fun x ++ y = BNA.add (x, y)
  fun x -- y =
      raise NotYetImplemented

  val sub = op--
end
