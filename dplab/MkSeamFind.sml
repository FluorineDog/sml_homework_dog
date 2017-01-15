functor MkSeamFind(structure Seq : SEQUENCE) : SEAMFIND =
struct
  structure Seq = Seq

  exception NotYetImplemented
 
  type 'a seq = 'a Seq.seq 
  open Seq

  type pixel = { r : real, g : real, b : real }
  type image = { width : int, height : int, data : pixel seq seq }
  type gradient = real

  fun generateGradients ({width, height, data}:image) = 
    let
      (*val _ = if length data <> height then raise NotYetImplemented else ()*)
      fun squaredis ({r=r1,g=g1,b=b1},{r=r2,g=g2,b=b2}) = 
        (r1-r2)*(r1-r2)+(g1-g2)*(g1-g2)+(b1-b2)*(b1-b2)
      fun calc (w,h):gradient = 
        let
          val mid = nth (nth data h) w
          val right = nth (nth data h) (w+1)
          val down = nth (nth data (h+1)) w
        in
          Math.sqrt (squaredis (mid,right) + squaredis(mid, down))
        end
    in
      tabulate (fn h=>tabulate (fn w=>
      if h=height-1 then 0.0 else if w = width-1 then 1.0E50 else calc(w,h)
      ) width) height
    end

             (*previous value*)
  fun pint x= print("&"^Int.toString x)
  fun findSeam G = 
    let 
      fun iterih f b s = case iter f b s of (body, tail) => 
        append(drop (body,1), singleton tail)
      val height = length G
      val width = length (nth G 0) 
      val base = tabulate (fn _=> (~1, 0.0)) width
      fun choose line w = let 
        val i2 = if w = 0 orelse nth line (w-1) > nth line w then w else w-1
        val i3 = if w = width-1 then i2 
          else if nth line i2 > nth line (w+1) then w+1 else i2
        in
          i3
        end
      val _=pint 1
      fun move ((S:gradient seq, L:int seq list), M:gradient seq) = 
        let
          val newL = tabulate (choose S) width
          val L' = newL::L
          val S' = map2 (fn (n, g)=> nth S n + g) newL M
        in
          (S',L')
        end
      val _=pint 2
      val _=pint (length G)
      val (lastline,prelist) = iter move ((nth G 0), nil) (drop (G,1))
      val _ = pint (List.length prelist)

      val _=pint 3
      val (minIndex,_) = reduce (fn ((n1,g1),(n2,g2))=> 
        if g1>g2 then (n2,g2) else (n1,g1)) (0,1E50) (enum lastline)
      val finalSeams = foldl (fn(L, seams)=>(nth L (hd seams))::seams)
        (minIndex::nil) prelist
      
      val _ = pint (List.length finalSeams)
      val _ = if List.length finalSeams <> height then raise NotYetImplemented else ()
      val _ = pint 4

    in
      fromList finalSeams
    end

end

