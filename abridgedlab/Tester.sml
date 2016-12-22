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

  structure NumOut = IntElt
  structure NumIn = EdgeSeqElt
  structure OutNeighborsOut = IntSeqElt
  structure OutNeighborsIn = EdgeSeqVertElt

  structure Bridges : BRIDGES =
    MkBridges(structure STSeq = MkSTSequence(structure Seq = ArraySequence))

  functor MkAStar(structure Vtx : HASHKEY) : ASTAR =
    MkAStarCore(structure Table = MkTreapTable(structure HashKey = Vtx)
                structure PQ = MkSkewBinomialHeapPQ(structure OrdKey = RealElt))

  structure IntAStar : ASTAR =
    MkAStar(structure Vtx = IntElt)

  structure StringAStar : ASTAR =
    MkAStar(structure Vtx = StringElt)

  
  (* Put stuff here to test your implementations! *)
  fun testBridges () = 
    let 
      val tests = fromList Tests.tests 
      val refOutput = fromList Tests.result
      val bridgeF = Bridges.findBridges o Bridges.makeGraph
      
      val pack = map2 (fn (testcase, answer)=>(testcase, Result.Value answer)) tests refOutput
      val checker = Checker.fromOutput (bridgeF, EdgeSeqElt.equal)
      val logger = Logger.create (EdgeSeqElt.toString o #1, EdgeSeqElt.toString)
      
    in
      Tester.testGroup checker logger (toList pack)
    end
end
