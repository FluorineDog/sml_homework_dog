structure Tests =
struct

  val testBridge = List.map ArraySequence.% [
      (*[(1,2),(2,3),(1,3),(1,4)],*)
      [],
      [(0,1),(1,2),(0,2),(2,3)],
      [(0,1),(1,2),(2,0),(2,3)],
      [(3,1),(4,2),(0,1),(1,2),(2,6),(6,0),(3,4),(4,5),(6,8),(8,7),(8,9),(9,10),(10,11),(9,11)],
      []
  ]
  val resultBridge = List.map ArraySequence.% [
      [(4,1)],
      [],
      [],
      [],
      []
  ]
  val heris = Real.fromInt
  
  val testAStar = List.map 
  (fn (edges, (S, T),h) => (ArraySequence.% edges, (ArraySequence.% S, ArraySequence.% T),h)) 
  [
      ([(1,2,0.2)],([1],[2]),heris)
  ]
  val resultAStar = List.map ArraySequence.% [
      [SOME (2,0.2)]
  ]
end
