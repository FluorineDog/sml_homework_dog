structure Tests =
struct
  val testsAdd : (IntInf.int * IntInf.int) list = [
    (4000,3334),
    (3,2),
    (123, 937),
    (1023, 1),
    (1023,1023),
    (1022, 1),
    (0,0),
    (156153534500,56233686565)
  ]

  val testsSub : (IntInf.int * IntInf.int) list = [
    (4000,3334),
    (3,2),
    (1024, 937)
  ]

  val testsMul : (IntInf.int * IntInf.int) list = [
    (156153534500,56233686565),
    (4000,3334),
    (8, 1),
    (1, 8),
    (3,2),
    (123, 937)
  ]

end
