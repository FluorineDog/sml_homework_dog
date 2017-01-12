functor MkBoruvkaSegmenter
  (structure Seq : SEQUENCE
   structure Rand : RANDOM210
   sharing Seq = Rand.Seq)
  : SEGMENTER =
struct
  structure Seq = Rand.Seq
  open Seq

  structure R = Rand
  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  (* Remove this exception when you're done! *)
  (*exception NotYetImplemented*)
  exception nyi
  fun findSegments (E_, n_) initial_credit = 
    let
       
    in
      raise nyi
    end
    (*raise NotYetImplemented*)
end