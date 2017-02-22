structure Tester =
struct
  open ArraySequence
  open StudentTestSuite

  (* * * loggers * * *)



  structure Bridges : BRIDGES =
    MkBridges(structure STSeq = MkSTSequence(structure Seq = ArraySequence))
  structure RefBridges : BRIDGES =
    MkRefBridges(structure STSeq = MkSTSequence(structure Seq = ArraySequence))

  functor MkAStar(structure Vtx : HASHKEY) : ASTAR =
    MkAStarCore(structure Table = MkTreapTable(structure HashKey = Vtx)
                structure PQ = MkSkewBinomialHeapPQ(structure OrdKey = RealElt))

  structure IntAStar : ASTAR =
    MkAStar(structure Vtx = IntElt)

  structure StringAStar : ASTAR =
    MkAStar(structure Vtx = StringElt)

  structure IntAstarElt = MkAStarElt(structure BaseElt = IntElt)

  fun orderseal f (a,b)= if f (a,b) then LESS else if f(b,a) then GREATER else EQUAL
  (* Put stuff here to test your implementations! *)
  fun testBridges () = 
    let
      open IntAstarElt
      val tests = fromList Tests.testBridge
      val refOutput = fromList Tests.resultBridge
      fun regulate s = sort EdgeElt.compare (map (fn (a,b)=>(Int.min (a,b), Int.max(a,b))) s)
      (*fun regulate s = s*)
      val bridgeF = regulate o Bridges.findBridges o Bridges.makeGraph
      val refbridgeF = regulate o RefBridges.findBridges o RefBridges.makeGraph
      
      val pack = map2 (fn (testcase, answer)=>(testcase, Result.Value (regulate answer))) tests refOutput
      val checker = Checker.fromOutput (bridgeF, EdgeSeqElt.equal)
      val checker2 = Checker.fromRefsol (bridgeF, refbridgeF, EdgeSeqElt.equal)
      val logger = Logger.create (EdgeSeqElt.toString o #1, EdgeSeqElt.toString)
      val logger2 = Logger.create (EdgeSeqElt.toString, EdgeSeqElt.toString)
      val _ = ignore (Tester.testGroup checker logger (toList pack))
      val _ = print "\n*** *** *** *** ***\n"
      val _ = Tester.testGroup checker2 logger2 (Tests.testBridge)
      (*val _ = map (print o (fn x => x^"\n") o GraphSeqElt.toString o Bridges.makeGraph) tests*)
    in
      ()
    end
  exception nyi
  fun testAStar () = 
    let
      open IntAstarElt
      open IntAStar
      val tests:AStarSeqElt.t = fromList Tests.testAStar
      val refOutput:PathElt.t seq = fromList Tests.resultAStar
      fun gen s = case s of (seqWE, (seqS,seqT), h) => 
        (findPath h o makeGraph) seqWE (Set.fromSeq seqS,Set.fromSeq seqT) 
      val pack = map2 (fn (testcase, answer)=>(testcase, Result.Value answer)) tests refOutput
      
      val checker = Checker.fromOutput(gen, PathElt.equal)
      val logger = Logger.create(AStarElt.toString o #1, PathElt.toString)
      val _ = Tester.testGroup checker logger (toList pack)
    in
      ()
    end
end
