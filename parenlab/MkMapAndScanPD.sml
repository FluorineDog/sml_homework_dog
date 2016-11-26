functor MkMapAndScanPD(structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq

  fun parenDist (parens : paren seq) : int option =
    let
 
      val value_parens = map (fn CPAREN=> (~1,1)|OPAREN=>(1,1)) parens
      val (ranktablePart, final)  =  scan (fn ((a,b),(c,d))=>(a+c, b+d)) (0,0) value_parens

      val ranktable = append(ranktablePart, singleton final)
      val zero_cparens = filter (fn (a,_)=>a<=0) ranktable
 
      val level_part = map (fn (a, _)=>a) zero_cparens
    in
      if 0 > reduce (fn(a,b)=>if a=0 andalso b=0 then 0 else ~1) 0 level_part
        orelse #1 final <> 0 orelse length parens = 0 then NONE 
      else let
        val rank_part = map (fn(_,b)=>b) zero_cparens
        val final_table = 
          tabulate (fn n =>((nth rank_part (n+1)) - (nth rank_part n))) (length rank_part - 1) 

        val result = reduce (fn(a,b)=>if a<b then b else a) 0 final_table
      in 
        SOME (result-2)
      end 
    end
end