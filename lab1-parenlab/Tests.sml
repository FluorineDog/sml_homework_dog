structure Tests =
struct
  (* Add your test cases to this list: *)
  open StudentTestSuite
  open ArrayParenPackage

  type 'a sequence  = 'a Seq.seq
  open Seq
  val tmp = let
    fun ap x = append((map (fn c=>"("::c) x),(map (fn c=>")"::c) x))
    fun kill 0 = singleton nil | kill n = (ap o ap) (kill (n-1)) 
  in
    map String.concat (kill 6)
  end
  
  val tests2 = [
    "(()()()((()(()((())))(()))))",
    "()()",
    "(()))",
    "(()()())()()()()",
    "(((())))",
    "))(())",
    "(())(())",
    ""
  (* 0123456789012345678901*)
  ]
  val tests = 
  (toList tmp) @ 
  tests2
end
