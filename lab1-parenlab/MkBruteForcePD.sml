functor MkBruteForcePD(structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq

  (* Remove this line when you're done. *)
  fun parenDist (parens : paren seq) : int option = 
  let
    val inte = Int.toString
    val pri_check = fn ((NONE, _)|(SOME 0, CPAREN)) => NONE
      | (SOME n, OPAREN) => SOME (n+1)
      | (SOME n, CPAREN) => SOME (n-1)
    val isValid = fn paren_ => (SOME 0) = (iter pri_check (SOME 0) paren_ ) 
  in 
    if(isValid(parens) = false orelse length parens = 0) then NONE else 
    SOME let
      val max = fn (a,b)=>if a<b then b else a 
      val n = length parens
    in 
      reduce max 0 (
        tabulate 
        (
          fn beg => reduce max 0 (
            tabulate 
            (
              fn tmp=> 
                let 
                  val size = tmp + 2
                  (*val s = print(inte(beg)^" "^inte(size)^"\n")*)

                in
                  if 
                    nth parens beg=OPAREN 
                    andalso nth parens (beg + size-1)=CPAREN 
                    andalso isValid(subseq parens ((beg+1), (size-2)))
                  then size - 2 else 0
                end
            )
            (n-beg-1)
            )
        )
        (n - 1) 
        )
    end
  end
end
