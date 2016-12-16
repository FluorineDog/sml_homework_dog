functor MkThesaurusASP (ASP : ALL_SHORTEST_PATHS where type vertex = string)
  : THESAURUS =
struct
  structure Seq = ASP.Seq
  open Seq

  (* Remove the following two lines when you're done! *)
  exception NYI
  type nyi = unit

  (* You must define the following type and
   * explain your decision here with a comment.
   *)
  type thesaurus = ASP.graph

  (* Task 3.1 *)
  fun make (S : (string * string seq) seq) : thesaurus =
    let
      val edgesSet = 
        map (fn (src, destSet)=> map (fn dest=>(src, dest)) destSet) S
      val edges = flatten edgesSet
    in
      ASP.makeGraph edges
    end

  (* Task 3.2 *)
  fun numWords (T : thesaurus) : int =
    ASP.numVertices T

  fun synonyms (T : thesaurus) (w : string) : string seq =
    ASP.outNeighbors T w 

  (* Task 3.3 *)
  fun query (T : thesaurus) (w1 : string) (w2 : string) : string seq seq =
    let 
      val asp = ASP.makeASP T w1
      val reportDest = ASP.report asp 
    in
      reportDest w2
    end
end
