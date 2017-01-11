structure Main = 
struct
  exception NYI
  val s = 1;
  type F = int->int
  type SF = F->F
  datatype 'a REF = WRAP of 'a->'a
  type Intref = F REF
  val fact:SF = fn f:F => (fn n:int => if n = 0 then 1 else n * f (n-1))
  val rec fix :SF->F = fn g:SF => g (fix g)
  val Y:SF->F = fn g:SF =>
    let 
      datatype 'a mu = Roll of ('a mu -> 'a)
      fun unroll (Roll x) = x
      type imu = F mu
      val s:imu->F = (fn x:imu => g (unroll x x))
    in
      s (Roll s)
    end 

end