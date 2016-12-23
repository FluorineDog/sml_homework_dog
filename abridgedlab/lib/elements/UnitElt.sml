structure UnitElt : ELEMENT =
struct
  type t = int->real

  exception NYI

  val default = Real.fromInt
  fun equal (x,y) = true
  fun compare (x,y) = EQUAL
  fun hash x = 0
  fun toString (_ : t) = "(h)"
end