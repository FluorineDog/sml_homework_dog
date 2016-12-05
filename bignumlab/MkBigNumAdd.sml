functor MkBigNumAdd(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util
  open Seq
  
  (* Remove this line when you're done. *)
  infix 6 ++
  datatype carry = GEN | PROP | STOP

  fun simplify res = 
    let
      val index_seq = mapIdx (fn (_,ZERO)=>~1|(n,ONE)=>n) res
      fun copy (a,0) = a
        | copy (_,b) = b
      val len = 1 + reduce copy ~1 index_seq 
    in     
      take (res, len)
    end
    
   fun x ++ y =
    let 
      val lx = length x 
      val ly = length y 
      val lm = Int.max (lx, ly)
      fun at con index = 
        if index < (length con ) then nth con index
        else ZERO
      val raw = tabulate 
                (fn n=> case (at x n, at y n) of (ZERO, ZERO) => STOP
                                              | ((ZERO,ONE)|(ONE,ZERO))=>PROP
                                              | (ONE,ONE)=>GEN
                ) (lm+1)
      fun copy (a, PROP) = a
        | copy (_, b)    = b  
      val data0= scani copy STOP raw
      val data = enum data0
      val data2= filterIdx (fn(i, x) => i=0 orelse #2 (nth data (i-1)) <> #2 x) data
      val data3= if #2 (nth data2 0) = STOP then drop(data2, 1) else data2
      val data4= map (fn (i, _)=>(i,PROP)) data3
      val patch= inject data4 data0
      fun finalfix (a,PROP) = if a=STOP then ONE else ZERO
        | finalfix (a,STOP) = if a=STOP then ZERO else ONE 
        | finalfix (a,GEN)  = if a=PROP then ZERO else ONE   
      val result = map finalfix (zip raw patch)
    in
      simplify result
    end

  val add = op++
end


(*functor MkBigNumAdd(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util
  open Seq

  (* Remove this line when you're done. *)
  (*exception NotYetImplemented*)

  infix 6 ++
  datatype carry = GEN | PROP | STOP

  fun x ++ y =
    let 
      val lx = length x 
      val ly = length y 
      val lm = Int.max (lx, ly)
      fun at con index = 
        if index < (length con ) then nth con index
        else ZERO
      val raw = tabulate (fn n=>(at x n, at y n)) (lm+1)
      fun copy (_,(ZERO,ZERO))=(ZERO,ZERO)
        | copy (_,(ONE,ONE))=(ONE,ONE)
        | copy (a, _)=a
      val data0 =scani copy (ZERO, ZERO) raw
      val data = enum data0
      val data2= filterIdx (fn(i, x) => i=0 orelse #2 (nth data (i-1)) <> #2 x) data
      val data3= if #2 (nth data2 0) = (ZERO,ZERO) then drop(data2, 1) else data2
      val data4= map (fn (i,(a, b))=>(i,(a, if b = ZERO then ONE else ZERO))) data3
      val patch= inject data4 data0
      fun finalfix (_,(ZERO, ONE)) = ONE 
        | finalfix (_,(ONE, ZERO)) = ZERO
        | finalfix ((a, b), (ONE, ONE))= if a=ZERO orelse b=ZERO then ZERO else ONE 
        | finalfix ((a, b), (ZERO, ZERO))= if a=ONE orelse b=ONE then ONE else ZERO
      val result = map finalfix (zip raw patch)
    in
      if nth result (length result - 1) = ZERO then take (result, (length result - 1)) else result 
    end
      (*raise NotYetImplemented*)

  val add = op++
end*)

