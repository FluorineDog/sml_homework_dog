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
  type mapping_t = vertex seq
  type credit_t = int seq

  (*fun pint n = print ((Int.toString n)^"$")*)
  (* Remove this exception when you're done! *)
  (*exception NotYetImplemented*)
  fun pint x = print ((Int.toString x)^"$")
  fun pint2 x = print ((Int.toString x)^"#")
  exception nyi
  fun findSegments (E_, n) initial_credit = 
  let
    fun null_edges N = tabulate (fn _=>(~1,~1,1000000)) N
    fun mask (mapping:mapping_t) (v:vertex):vertex = nth mapping v
    (*EE is sorted decreasingly*)
    (*R is list of MST edges*)
    (*mapping contains SSC-index for each vertex*)
    fun checkEdge valueF =  
      fn (u,v,w) => Int.min(valueF u, valueF v) >= w 
        
    fun helper (R:credit_t) (M:mapping_t) (EE0:edge seq) (N:int) (r:Rand.rand):mapping_t =
    let 
      (*$$ is base mapping. $$:vertex_original_index=>SCC_index*)
      val $$ = mask M
      val value = mask R o $$
      val EE = filter (fn(u,v,w)=>($$ u <> $$ v andalso checkEdge value (u,v,w))) EE0
      (*use given algorithm (from pdf) to get light edges*)
      val injection = map (fn (EDGE:edge)=>(($$ o #1) EDGE, EDGE)) EE
      val light_edges = inject injection (null_edges N)
      (*filter Tail=>Head edges*)
      val coinResult = Rand.flip r N
      fun isHead u = nth coinResult ($$ u) = 0
      val starlines = filter 
      (fn(u,v,_)=>v <> ~1 andalso isHead v andalso (not o isHead) u) light_edges
      (*rule out Tails vertex that is on a valid edge*)
      val injectionStar = map (fn(u,v,_)=>($$ u,0)) starlines
      val HeadsAndUnuseTails = inject injectionStar (tabulate (fn _=>1) N)
      (*use it to generate bottomMapping: SCC_index=>new_SCC_index*)
      (*and N': total number of new_SCC*)
      val (bottomMapping,N') = scan op+ 0 HeadsAndUnuseTails
      (*Tails should be bound to corresponding Heads*)
      (*middle mapping: Tail_SCC_index =>Head_SCC_index*)
      val injectionMapping = map (fn(u,v,_)=>($$ u, $$ v)) starlines
      val middleMapping = inject injectionMapping (tabulate (fn scc=>scc) N)
      (*composite mappings together*)
      (*## = new_$$*)
      val NewFromOld  = mask bottomMapping o mask middleMapping
      val M' = tabulate (NewFromOld o $$) n
      val ## = mask M'
      (*credits with sequence*)
      local 
        val valueScc = mask R
        val SCC_info = (collect Int.compare o map (fn(u,v,w)=>($$ v,(value u,w))) ) starlines
        val join = reduce (fn((r1, w1),(r2,w2))=>(Int.min (r1,r2), w1+w2)) (initial_credit, 0)
        val injectionNewSCC = map (fn (scc,S)=>(case join S of(r,w)=>
          (NewFromOld scc, Int.min (valueScc scc, r)-w )
            (*before pint r before pint(valueScc scc) before pint2 w before print "\t"*)
            )) SCC_info
        (*val _= print "\n"*)
        val injectionOldSCC = mapIdx (fn (v, r)=>(mask bottomMapping (mask middleMapping v), r)) R
        val null_R = tabulate (fn _=> ~10000000) N'
      in
        
        val R' = inject (append(injectionOldSCC, injectionNewSCC)) null_R
        (*val _ = map (fn x=> if x = ~10000000 then raise nyi else ()) R'*)
      end
      (*filter out edges in the same new_SCC*)
      (*val EE' = filter (fn(u,v,w)=>(## u <> ## v andalso checkEdge R' ## (u,v,w))) EE*)
      (*next random seed*)
      val r' = Rand.next r
    in          
      if length EE = 0 then M else

      helper R' M' EE N' r'
    end
      
    (*Sort edges decreasingly*)
    val R0 = tabulate (fn _ => initial_credit) n
    val M0 = tabulate (fn x=>x) n
    (*val Evalid = filter(checkEdge R0 (mask M0)) E_*)
    val E0 = sort (Int.compare o (fn(a:edge,b:edge)=>(#3 b,#3 a))) E_
  in
    helper R0 M0 E0 n (Rand.fromInt 15210) 
  end
end