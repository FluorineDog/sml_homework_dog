functor MkSeamFind(structure Seq : SEQUENCE) : SEAMFIND =
struct
  structure Seq = Seq

  exception NotYetImplemented
 
  type 'a seq = 'a Seq.seq 

  type pixel = { r : real, g : real, b : real }
  type image = { width : int, height : int, data : pixel seq seq }
  type gradient = real

  fun generateGradients {width, height, data} = raise NotYetImplemented

  fun findSeam G = raise NotYetImplemented

end

