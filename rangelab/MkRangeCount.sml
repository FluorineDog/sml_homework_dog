functor MkRangeCount(structure OrdTable : ORD_TABLE) : RANGE_COUNT =
struct
  structure OrdTable = OrdTable
  open OrdTable

  (* Ordered table type: *)
  type 'a table = 'a table
  type 'a seq = 'a Seq.seq
  type point = Key.t * Key.t

  (* Use this to compare x- or y-values. *)
  val compareKey : (Key.t * Key.t) -> order = Key.compare

  (* Remove this before submitting! *)
  exception NYI
  exception Duplicate

  (* Define this yourself *)
  (*type 'a countTable = ('a table) seq*)
  type countTable = unit table table

  
  fun makeCountTable (S : point seq) : countTable =
    let
      val PointTable = fromSeq S
      fun insertKey (T:unit table, (_, y:Key.t)):unit table = 
        case split (T,y) of (l,NONE,r)=> join(l, join(singleton (y,()), r))
        (*insert (fn _=>raise Duplicate) (y,()) T *)
     in
      iterih insertKey (empty()) PointTable
    end
  (*fun pixxt x= print ("&"^(Int.toString x)^"$")*)
  fun count (T : countTable)
                   ((xLeft, yHi) : point, (xRght, yLo) : point) : int  =
    let
      fun countEdgeAndLeft Edge = case Edge of NONE => ~1000
        | SOME PT => size (getRange PT (yLo, yHi))

      val leftEdge = Option.map #2 (previous T xLeft)
      val leftExc = countEdgeAndLeft leftEdge 
      val rightEdges = case find T xRght of SOME PT =>SOME PT
        | NONE => Option.map #2 (previous T xRght)
      val rightInc = countEdgeAndLeft rightEdges
      (*val _ = iter (pixxt o size o #2 o #2) () T*)
      val _ = iter (pixxt o countEdgeAndLeft o SOME o #2 o #2) () T
    in
      rightInc - leftExc
    end
end
