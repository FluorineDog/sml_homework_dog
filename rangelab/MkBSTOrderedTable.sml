functor MkBSTOrderedTable (structure Tree : BSTREE
                           structure Seq : SEQUENCE) : ORD_TABLE =
struct

  structure Table = MkBSTTable(structure Tree = Tree
                               structure Seq = Seq)

  (* Include all the functionalities of the standard Table *)
  open Table

  (* This is defined after "open" so it doesn't get overwritten *)
  structure Key = Tree.Key
  type key = Key.t

  type 'a result = (key * 'a) option
  (* Remember, type 'a table = 'a Tree.bst *)
  (* Remove this line before submitting! *)
  exception NYI

  fun first (T : 'a table) : (key * 'a) option =
    case Tree.expose T of NONE => NONE
      | SOME N' =>
      let
        fun helper {left, key, value, ...} = 
          case Tree.expose left of NONE => SOME (key, value)
            | SOME N => helper N
      in
        helper N'
      end
    


  fun last (T : 'a table) : (key * 'a) option =
    case Tree.expose T of NONE => NONE
      | SOME N' =>
      let
        fun helper {key, value, right,...} = 
          case Tree.expose right of NONE => SOME (key, value)
            | SOME N => helper N
      in
        helper N'
      end
    

  (*based on two cases*)
  (*1. key has left subtree*)
  (*2. key doesn't have left sub tree*)
  fun previous (T : 'a table) (k : key) : (key * 'a) option =
    let
      val _ = ()
      (*R is the last node where it turns right*)
      fun helper tree (R:'a result):'a result= 
        case Tree.expose tree of 
          NONE => R 
        | SOME {left, key, value, right} => 
          case Key.compare (k, key) of
            EQUAL=>let val R' = last left 
                   in if Option.isSome R' then R' else R end
          | LESS =>helper left R
          | GREATER=>helper right (SOME(key, value))
    in 
      helper T NONE
    end

  (*based on two cases*)
  (*1. key has right subtree*)
  (*2. key doesn't have right subtree*)
  fun next (T : 'a table) (k : key) : (key * 'a) option =
    let
      val _ = ()
      (*R is the last node where it turns left*)
      fun helper tree (R:'a result):'a result= 
        case Tree.expose tree of 
          NONE => R 
        | SOME {left, key, value, right} => 
          case Key.compare (k, key) of
            EQUAL=>let val R' = last right 
                    in if Option.isSome R' then R' else R end
          | GREATER=>helper right R
          | LESS =>helper left (SOME(key, value))
    in 
      helper T NONE
    end

  fun join (L : 'a table, R : 'a table) : 'a table =
    raise NYI

  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table =
    raise NYI

  fun getRange (T : 'a table) (low : key, high : key) : 'a table =
    raise NYI

end
