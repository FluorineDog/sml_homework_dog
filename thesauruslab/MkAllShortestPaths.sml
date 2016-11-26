functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Remove the following when you're done! *)
  exception NYI
  type nyi = unit

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  (* You must define the following two types and
   * explain your decision here with comments.
   *)
  type graph = nyi
  type asp = nyi

  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph =
    raise NYI

  (* Task 2.2 *)
  fun numEdges (G : graph) : int =
    raise NYI

  fun numVertices (G : graph) : int =
    raise NYI

  (* Task 2.3 *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
    raise NYI

  (* Task 2.4 *)
  fun makeASP (G : graph) (v : vertex) : asp =
    raise NYI

  (* Task 2.5 *)
  fun report (A : asp) (v : vertex) : vertex seq seq =
    raise NYI
end
