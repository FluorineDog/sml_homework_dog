structure Tester =
struct
  open ArraySequence
  open StudentTestSuite

  (* * * loggers * * *)
  structure EdgeElt = MkPairElt(structure EltA = IntElt
                                structure EltB = IntElt)
  structure EdgeSeqElt = MkSeqElt(structure Elt = EdgeElt
                                  structure Seq = ArraySequence)
  structure EdgeSeqVertElt = MkPairElt(structure EltA = EdgeSeqElt
                                       structure EltB = IntElt)
  structure IntSeqElt = MkSeqElt(structure Elt = IntElt
                                 structure Seq = ArraySequence)
  structure StringPairElt = MkPairElt(structure EltA = StringElt
                                      structure EltB = StringElt)
  structure StringSeqElt = MkSeqElt(structure Elt = StringElt
                                    structure Seq = ArraySequence)

  structure GraphSeqElt = MkSeqElt(structure Elt = IntSeqElt
                                 structure Seq = ArraySequence)

  structure NumOut = IntElt
  structure NumIn = EdgeSeqElt
  structure OutNeighborsOut = IntSeqElt
  structure OutNeighborsIn = EdgeSeqVertElt

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

  fun orderseal f (a,b)= if f (a,b) then LESS else if f(b,a) then GREATER else EQUAL
  (* Put stuff here to test your implementations! *)
  fun testBridges () = 
    let 
      val tests = fromList Tests.tests 
      val refOutput = fromList Tests.result
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
      val _ = Tester.testGroup checker2 logger2 (Tests.tests)
      (*val _ = map (print o (fn x => x^"\n") o GraphSeqElt.toString o Bridges.makeGraph) tests*)
    in
      ()
    end
end
