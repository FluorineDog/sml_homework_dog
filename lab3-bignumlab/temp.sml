  fun simplify res = 
    let
      val index_seq = mapIdx (fn (_,ZERO)=> (~1) |(n,ONE)=>n) res
      fun copy (a,0) = a
        | copy (_,b) = b
      val len = 1 + reduce copy ~1 index_seq 
    in     
      take (res, len)
    end
