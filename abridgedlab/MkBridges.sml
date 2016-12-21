functor MkBridges(structure STSeq : ST_SEQUENCE) : BRIDGES =
struct
  structure Seq = STSeq.Seq
  open Seq
  structure ST = STSeq

  type vertex = int
  type edge = vertex * vertex
  type edges = edge seq

  (* Remove these two lines before submittting. *)
  exception NotYetImplemented

  type ugraph = (vertex seq) seq

  fun makeGraph (E : edge seq) : ugraph = 
    let 
      val biedges = append (E,(map (fn (src, dest)=>(dest,src)) E))
      val max = reduce Int.max ~1 (map #1 biedges)
      val nullseq:ugraph = tabulate (fn _=>empty()) (max+1)
      val graphtable = collect Int.compare biedges 
    in
      inject graphtable nullseq
    end 
  
  fun construct (id:'a) (n:int):'a ST.stseq = (ST.fromSeq o tabulate (fn _=>id)) n
  infix 8 @@
  fun op@@ (container, n) = ST.nth container n
  fun findBridges (G : ugraph) : edges = 
    let 
      val size = length G
      val inf = size*2+1
      (*            disc,fin,parent,min_reach *)
      type State = int*int*vertex*int
      type StateSeq = State ST.stseq
      type discSeq = int option ST.stseq
      fun DFS ((p, S:discSeq, R:StateSeq, min_reached, t),cur) = 
        case S@@cur of SOME disc => (p, S, R, Int.min(min_reached,disc), t)
        | NONE =>
          let 
            val NG = nth G cur
            val NGkP = filter (fn x=>x<>p) NG
            val S' = ST.update (cur, SOME t) S
            val (_, S'', R', new_min, new_t) = iter DFS (cur, S', R, inf, t+1) NGkP
            val final_min = Int.min (new_min, t)
            val R' = ST.update (cur, (t, final_min, p, new_min)) R
          in
            (p, S'', R', final_min, new_t+1)
          end
      val (_,_,final_record,_,_) =
        DFS ((0, construct NONE size, construct (0,0,0,0) size, inf, 0),0)
      val extractEdges =
        let 
          fun convertor (dest, (disc,_,parent,min)) = 
            ((parent, dest),min=disc andalso dest<>0)
        in
          mapIdx convertor (ST.toSeq final_record)
        end
    in
      (map #1 o filter #2 ) extractEdges 
    end
end
