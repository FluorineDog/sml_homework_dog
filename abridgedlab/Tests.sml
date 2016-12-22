structure Tests =
struct

  val tests = List.map ArraySequence.% [
      [(1,2),(2,3),(1,3),(1,4)]
  ]
  val result = List.map ArraySequence.% [
      [(1,4)]
  ]
end
