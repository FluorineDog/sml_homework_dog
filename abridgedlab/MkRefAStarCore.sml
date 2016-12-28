functor MkAStarCore(structure Table : TABLE
                    structure PQ : PQUEUE
                      where type Key.t = real) : ASTAR =
struct
  structure Set = Table.Set
  structure Seq = Set.Seq

  type weight = real
  type vertex = Set.key
  type edge = vertex * vertex * weight
  type heuristic = vertex -> real
  type 'a table = 'a Table.table


  (* Define this type yourself *)
  type graph = (weight table) table
  
  fun makeGraph (E : edge Seq.seq) : graph =
    let
      val pairTbl = Table.collect (Seq.map (fn (v1,v2,w) => (v1,(v2,w))) E)
    in
      Table.map Table.fromSeq pairTbl
    end
  (*A Simple modification of the Dijkstra algorithm*)
  fun findPath h G (S, T) = 
    let
     fun N(v) = case Table.find G v
                of NONE => Table.empty ()
                | SOME nbr => nbr
     fun astarhelper D Q =
            case PQ.deleteMin Q
            of (NONE, _) => D
            | (SOME (d, v), Q') =>
              case Table.find D v
              of SOME _ => astarhelper D Q'
              | NONE =>
                    let
                      val insert = Table.insert (fn (x, _) => x)
                      val D' = insert (v, d-(h v)) D
                      fun relax (q, (u, w)) = PQ.insert (d+w-(h v)+(h u), u) q
                      val Q'' = Table.iter relax Q' (N v)
                    in if (Set.find T v) then D' else astarhelper D' Q'' end
    val afterRunning = astarhelper(Table.empty()) (PQ.fromList (Seq.toList (Seq.map (fn i=> ((h i),i))(Set.toSeq S))))
    val possibleResults = Table.toSeq (Table.extract (afterRunning,T))
    in
       if (Seq.length possibleResults = 0) then NONE
       else SOME (Seq.nth possibleResults 0)
    end
end