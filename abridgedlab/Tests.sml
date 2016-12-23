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
  
  val heauris = Real.fromInt o (fn x => 4*(x div 10)) 
  fun doubleSide edges = ArraySequence.append((ArraySequence.map (fn (s,d,w)=>(d,s,w)) edges),edges)
  (*val k = doubleSide (ArraySequence.% [(1,2,0.2)])*)
  val testAStar = List.map (fn (edges, (S, T),h) => (doubleSide(ArraySequence.% edges), (ArraySequence.% S, ArraySequence.% T),h)) 
  [
      ([(1,2,0.2)],([1],[2]),fn 1=>0.1 | 2=>0.0 | _=> 10000.0),
      ([(1,2,0.2)],([1],[3]),fn 1=>0.1 | 2=>0.0 | _=> 10000.0),
      ([(1,2,0.2)],([0],[2]),fn 1=>0.1 | 2=>0.0 | _=> 10000.0),
      ( [
            (43,44,3.0),
            (44,31,4.0),(43,31,5.0),(43,30,5.0),
            (30,31,6.0),
            (31,20,5.0),(30,20,5.0),
            (20,12,5.0),(20,11,5.0),
            (12,01,4.0),(12,02,5.0),(11,02,5.0)
        ],([20],[1,2]),heauris),
      ( [
            (43,44,3.0),
            (44,31,4.0),(43,31,5.0),(43,30,5.0),
            (30,31,6.0),
            (31,20,5.0),(30,20,5.0),
            (20,12,5.0),(20,11,5.0),
            (12,01,4.0),(12,02,5.0),(11,02,5.0)
        ],([43,44],[01,02]),heauris),
      ( [
            (43,44,3.0),
            (44,31,4.0),(43,31,5.0),(43,30,5.0),
            (30,31,6.0),
            (31,20,105.0),(30,20,5.0),
            (20,12,5.0),(20,11,5.0),
            (12,01,4.0),(12,02,5.0),(11,02,5.0)
        ],([31],[01,02]),(fn x=>1000.0*x) o heauris)
  ]
  val resultAStar = 
      SOME (2,0.2)::
      NONE::
      NONE::
      SOME (1,9.0)::
      SOME (1,18.0)::
      SOME (1,114.0):: (* Note: test for inadmissible heauris*)
      nil
end