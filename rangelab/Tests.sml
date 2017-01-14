structure Tests =
struct
  structure Seq = ArraySequence
  open Seq

  type point = int * int

  (* Here are a couple of test structures. ordSet1 is a test ordered set
   * (table), while points1 is a test collection of points in the plane.
   *
   * Note that for an ordered table test, you should just specify a sequence
   * of keys (and values will be automatically set to unit). *)
  structure RD = Random210

  val ordSet2 = % [5, 7, 2, 8, 9, 1]
  val oo = %[0,2,3,4,1]
  val ordSet1 = RD.randomIntSeq (RD.fromInt 0) (SOME (0,100)) 30
  val ordSet3 = RD.randomIntSeq (RD.fromInt 10) (SOME (1001,2000)) 100
  val s = toList(map (fn e=>(ordSet1, e)) ordSet1)
  val s1 = toList(map (fn e=>(ordSet1, e+1)) ordSet1)
  val s2 = toList(map (fn e=>(ordSet1, e-1)) ordSet1)
  val ss = flatten (tabulate (fn e=>tabulate (fn f=> (ordSet1,(e,e+f))) 100) 100)

  val testsFirst = [
    ordSet1,
    % []
  ]
  val testsLast = [
    ordSet1,
    % []
  ]
  val testsPrev = [
    (ordSet1, 8),
    (ordSet1, 1),
    (% [], 8)
  ]@s@s1@s2
  val testsNext = [
    (ordSet1, 8),
    (ordSet1, 100),
    (% [], 8)
  ]@s@s1@s2
  val testsJoin = [
    (% [], % [100]),
    (ordSet1, ordSet3),
    (ordSet1, % [~1]),
    (ordSet1, % [100001])
  ]
  val testsSplit = [
    (ordSet1, 7),
    (ordSet1, 100),
    (% [], 7)
  ]@s@s1@s2
  val testsRange = [
    (ordSet2, (5,8)),
    (ordSet2, (10,12)),
    (oo, (1,3)),
    (% [], (5,8))
  (*]@(toList ss)*)
  ]

  val points1 = % [(0,0),(1,2),(3,3),(4,4),(5,1)]
  val points2 : point seq = % []
  val points3 = % [(10000,10000),(0,0)]
  val points4 = tabulate (fn i => (i,i)) 1000

  val testsCount = [
    (points1, ((1,3),(5,1))),
    (points1, ((1,3),(5,2))),
    (points1, ((1,5),(5,1))),
    (points1, ((1,4),(5,1)))
    (*(points1, ((2,4),(4,2))),*)
    (*(points1, ((100,101),(101,100))),*)

    (*(points2, ((0,10),(10,0))),*)
    (*(points3, ((0,10000),(10000,0))),*)
    (*(points4, ((0,500),(1000,0)))*)
  ]


end
