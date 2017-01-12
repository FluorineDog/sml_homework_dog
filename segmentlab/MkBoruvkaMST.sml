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
  exception Byi of unit*unit
  fun pint n = print ((Int.toString n)^"$")
  fun MST (E : edge seq, n : int) : edge seq =
    let
      fun null_edges N = tabulate (fn _=>(~1,~1,1000000)) N
      fun mask (mapping:mapping_t) (v:vertex):vertex = nth mapping v

      (*EE is sorted decreasingly*)
      (*R is list of MST edges*)
      (*mapping contains SSC-index for each vertex*)
      fun helper (R:record_t) (M:mapping_t) (EE:edge seq) (N:int) (r:Rand.rand):record_t =
        if N = 1 then R else
        let
          val _ = () 
          (*$$ is base mapping. $$:vertex_original_index=>SCC_index*)
          val $$ = mask M
          (*use given algorithm (from pdf) to get light edges*)
          val injection = map (fn (EDGE:edge)=>(($$ o #1) EDGE, EDGE)) EE
          val light_edges = inject injection (null_edges N)
          (*filter Tail=>Head edges*)
          val coinResult = Rand.flip r N
          fun isHead u = nth coinResult ($$ u) = 0
          val starlines = filter (fn(u,v,_)=>isHead v andalso (not o isHead) u) light_edges
          (*Record with linked list*)
          val R' = starlines::R
          (*rule out Tails vertex that is on a valid edge*)
          val injectionStar = map (fn(u,v,_)=>($$ u,0)) starlines
          val HeadsAndUnuseTails = inject injectionStar (tabulate (fn _=>1) N)
          (*use it to generate bottomMapping: SCC_index=>new_SCC_index*)
          (*and N': total number of new_SCC*)
          val (bottomMapping,N') = scan op+ 0 HeadsAndUnuseTails
          (*Tails should be bound to corresponding Heads*)
          (*middle mapping: Tail_SCC_index =>Head_SCC_index*)
          val injectionMapping = map (fn(u,v,_)=>($$ u, $$ v)) starlines
          val middleMapping = inject injectionMapping (tabulate (fn x=>x) N)
          (*composite mappings together*)
          (*## = new_$$*)
          val M' = tabulate (mask bottomMapping o mask middleMapping o $$) n
          val ## = mask M'
          (*filter out edges in the same new_SCC*)
          val EE' = filter (fn(u,v,_)=>(## u <> ## v)) EE
          (*next random seed*)
          val r' = Rand.next r
        in
          helper R' M' EE' N' r'
        end
      (*Sort edges decreasingly*)
      val E'= sort (Int.compare o (fn(a,b)=>(#3 b,#3 a))) E
    in
      (flatten o fromList) (helper nil (tabulate (fn x=>x) n) E' n (Rand.fromInt 0))
    end
    (*raise NotYetImplemented*)

end
