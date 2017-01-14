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
      fun insertKey (T:unit table, (y:Key.t, _)):unit table = 
        insert (fn _=>raise Duplicate) (y,()) T
      val T = iterh insertKey 
    in
      raise NYI
    end

  fun count (T : countTable)
                   ((xLeft, yHi) : point, (xRght, yLo) : point) : int  =
    raise NYI
end
