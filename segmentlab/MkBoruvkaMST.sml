functor MkBoruvkaMST (structure Seq : SEQUENCE
                      structure Rand : RANDOM210
                      sharing Seq = Rand.Seq) : MST =
struct
  structure Seq = Rand.Seq
  open Seq

  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  (* Remove this exception when you're done! *)
  (*exception NotYetImplemented*)

  fun MST (E : edge seq, n : int) : edge seq =
    empty()
    (*raise NotYetImplemented*)

end
