functor MkBoruvkaMST (structure Seq : SEQUENCE
                      structure Rand : RANDOM210
                      sharing Seq = Rand.Seq) : MST =
struct
  structure Seq = Rand.Seq
  open Seq

  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight
  type mapping_t = vertex seq
  type record_t = edge seq List.list
  (* Remove this exception when you're done! *)
  (*exception NotYetImplemented*)

  fun MST (E : edge seq, n : int) : edge seq =
    let
      fun mask (mapping:mapping_t) (v:vertex):vertex = nth mapping v
      fun helper (R:record_t) (M:mapping_t) (EE:edge seq):edge seq =
        let
          val ## = mask M (*create mapping function*)
          val 
          (*filter*)
        in
          empty()
        end
    in
      fromList [(0,1,5),(0,2,10)]
    end
    (*raise NotYetImplemented*)

end
