functor MkBigNumAdd(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util
  open Seq

  (* Remove this line when you're done. *)
  exception NotYetImplemented

  infix 6 ++

  datatype carry = GEN | PROP | STOP

  fun x ++ y =
      raise NotYetImplemented

  val add = op++
end
