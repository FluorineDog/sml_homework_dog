functor MkAStarCore(structure Table : TABLE
                    structure PQ : PQUEUE
                      where type Key.t = real) : ASTAR =
struct
  structure Set = Table.Set
  structure Seq = Set.Seq
  open Seq
  structure T = Table
  type weight = real
  type vertex = Set.key
  type edge = vertex * vertex * weight
  type heuristic = vertex -> real

  (* Uncomment this line once you're done *)
  exception NotYetImplemented

  (* Define this type yourself *)
  type graph = weight T.table T.table
  fun makeGraph (E : edge Seq.seq) : graph = 
    (T.map T.fromSeq o T.collect o map (fn (s,d,w)=> (s,(d,w))))  E

  fun findPath h G (S, T) =      
    let
      fun N(v) =
          case Table.find G v
            of NONE => Table.empty ()
              | SOME nbr => nbr

      fun dijkstra' D Q:(vertex*weight)option =
          case PQ.deleteMin Q
            of (NONE, _) => NONE
              | (SOME (d, v), Q') => if Set.find T v then SOME (v,d) else
                case Table.find D v
                  of SOME _ => dijkstra' D Q'
                  | NONE =>
                    let
                      val insert = Table.insert (fn (x, _) => x)
                      val D' = insert (v, d) D
                      fun relax (q, (u, w)) = PQ.insert (d+w, u) q
                      val Q'' = Table.iter relax Q' (N v)
                    in dijkstra' D' Q''
                    end
    
      val sourceSet = (PQ.fromList o toList o map (fn x=>(0.0,x)) o Set.toSeq) S
      val final = dijkstra' (Table.empty ()) sourceSet
      (*(PQ.singleton (0.0, u))*)
    in
      (*raise NotYetImplemented*)
      final
    end
end
