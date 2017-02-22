functor MkSeqUtil(structure S : SEQUENCE) : SEQUENCE_UTIL =
struct
  structure Seq = S
  open Seq

  type 'a hist = ('a * int) seq

  (* Remove this line when you're done. *)
  (*exception NotYetImplemented*)
  exception OutOfRange
  fun tokens (cp : char -> bool) (str : string) : string seq =
    let
      val n = String.size str
      val chars = tabulate (fn i => (i, String.sub (str, i))) n
      val idx = map (fn (i,_) => i) (filter (fn (_,c) => cp c) chars)

      (* grab substrings in between delimiters *)
      val subs = map2 (fn (i,i') => String.substring (str, i, i' - i))
                      (append (singleton 0, map (fn i => i + 1) idx))
                      (append (idx, singleton n))
    in filter (fn s => size s > 0) subs
    end

  fun histogram (cmp : 'a ord) (s : 'a seq) : 'a hist =
    map (fn (a, c) => (a, length c))
        (collect cmp (map (fn a => (a, ())) s))
        
  fun /\ f a b = f(a,b)
  
  fun choose (hist : 'a hist) (p : real) : 'a = 
    if p>1.0 orelse p<0.0 then raise OutOfRange
    else
    let
      val id = #1 (nth hist 0)
      fun second_plus ((_,a),(s,b)) = (s,a+b)
      val tmpHist = scani second_plus (id,0) hist
      val standard = (Real.ceil o /\ op* p o Real.fromInt o #2 o nth tmpHist) (length tmpHist - 1)
      fun rightmostLesser (std) (a:'a*int, b) = if ((#2 a) >= std) then a else b
    in  
      #1 (reduce (rightmostLesser standard) (id,0) tmpHist)
    end 
end
