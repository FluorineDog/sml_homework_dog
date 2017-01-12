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
  exception Byi of int*int
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
          (*$$ is base mapping*)
          val $$ = mask M
          val injection = map (fn (EDGE:edge)=>(($$ o #1) EDGE, EDGE)) EE
          val light_edges = inject injection (null_edges N)
          val coinResult = Rand.flip r N
          fun isHead u = nth coinResult ($$ u) = 1
          val starlines = filter (fn(u,v,_)=>isHead u andalso (not o isHead) v) light_edges
          val R' = starlines::R
          val N'' = N - length starlines
          val injectionStar = map (fn(u,v,_)=>($$ v,0)) starlines
          val (bottomMapping,N') = scan op+ 0 (inject injectionStar (tabulate (fn _=>1) N))
          val _ = if(N''<>N') then raise Byi (N',N'') else ()
          val injectionMapping = map (fn(u,v,_)=>(v, mask M u)) starlines
          val M' = inject injectionMapping M 
          val M'' = map (mask bottomMapping) M' 
          val ## = mask M''
          val EE' = filter (fn(u,v,_)=>(## u <> ## v)) EE
          val r' = Rand.next r
          (*filter*)
        in
          helper R' M'' EE' N' r'
        end
    in
      (flatten o fromList) (helper nil (tabulate (fn x=>x) n) E n (Rand.fromInt 0))
    end
    (*raise NotYetImplemented*)

end
