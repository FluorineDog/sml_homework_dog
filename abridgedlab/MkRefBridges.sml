functor MkRefBridges(structure STSeq : ST_SEQUENCE) : BRIDGES =
struct
  structure Seq = STSeq.Seq
  type 'a stseq = 'a STSeq.stseq
  open Seq

  type vertex = int
  type edge = vertex * vertex
  type edges = edge seq

  type ugraph = (vertex seq) seq

  fun makeGraph (E : edge seq) : ugraph = 
    let val bidirGraph = append(E,(map (fn(i,j)=>(j,i)) E))
      val length = 1 + reduce Int.max 0 (map Int.max E)
      val toInject = collect Int.compare bidirGraph
    in inject toInject (tabulate (fn _=> empty()) length) end

  fun findBridges (G : ugraph) : edges = 
    let
      fun DFS p ((B,X,c,m),v) =
        if (isSome(STSeq.nth X v)) then (B,X,c,Int.min(m,valOf (STSeq.nth X v)))
        else 
          let
            val updX = STSeq.update (v,SOME c) X
            val toVisit = filter (fn v=> v<> p) (nth G v)
            val (fixB,fixX,fixc,fixm) = iter (DFS v) (B,updX,c+1,(length G)) toVisit
            val finalB = if p<>v andalso fixm >= c then (STSeq.update (v,singleton((p,v))) fixB) else fixB
          in 
            (finalB,fixX,fixc,Int.min(m,fixm)) 
          end
       val Vert = tabulate (fn i=>i) (length G)
       val X = STSeq.fromSeq (tabulate (fn _=>NONE) (length G))
       val B = STSeq.fromSeq (tabulate (fn _=>empty()) (length G))
       val res =  #1(iter (fn(S,v)=> DFS v (S,v)) (B,X,0,0) Vert)
       val final = flatten(STSeq.toSeq res)
    in 
      final
    end
end
