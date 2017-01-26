functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Remove the following when you're done! *)
  
  (*exception NYI*)
  exception NoData
  (*type nyi = unit*)

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  (* You must define the following two types and
   * explain your decision here with comments.
   *)

  (*graph is of adjacency list*)
  type graph = Set.set table
  (*asp is the reverse of asp DAG*)
  type asp = graph

  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph = 
    let
      val g = (Table.map Set.fromSeq o Table.collect) E
      val null_g = (fromSeq o map (fn(_,dest)=>(dest, Set.empty()))) E 
    in
      Table.merge (fn (first,_)=>first) (g, null_g) 
    end

  fun reverseGraph (G:graph) = 
    let
      fun tableSlot2seq (src, destSet) = 
        map (fn dest=>(src, dest)) (Set.toSeq destSet)
      val edges = (flatten o map tableSlot2seq o Table.toSeq) G 
      val rev_edges = map (fn (src, dest)=>(dest,src)) edges 
    in
      makeGraph rev_edges
    end

      
  (* Task 2.2 *)
  fun numEdges (G : graph) : int = 
    (Table.reduce op+ 0 o Table.map Set.size) G

  fun numVertices (G : graph) : int =
    (Table.size o reverseGraph) G
  
  (* Task 2.3 *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
    case Table.find G v of NONE=>raise NoData | SOME x=>Set.toSeq x

  (* Task 2.4 *)
  fun makeASP (G : graph) (v : vertex) : asp =
    let
      fun BFS (visited:graph) frontier = 
        let
          val frontG = Table.extract(G, frontier)
          fun isVisited v = 
            (Option.isSome o find visited) v orelse Set.find frontier v 
          val newEdgeG = Table.map (Set.filter (not o isVisited)) frontG
          val newF = Table.reduce Set.union (Set.empty()) newEdgeG
          val newVisited = Table.merge Set.union (newEdgeG, visited)
        in
          if Set.size newF = 0 then newVisited else BFS newVisited newF
        end
    in
      reverseGraph (BFS (Table.empty()) (Set.$ v))
    end

  fun reverseSeq s = 
    let val sz = length s in tabulate (fn n=>nth s (sz-1-n)) sz end

  (* Task 2.5 *)
  fun report (A : asp) (destination : vertex) : vertex seq seq = 
    let
      fun operate (vt:vertex) = 
        case outNeighbors A vt of NG => 
         if length NG = 0 
         then singleton (vt::nil) 
         else (map (fn body=>vt::body) o flatten o map operate) NG
    in
      (map (reverseSeq o Seq.fromList) o operate) destination
    end
end