functor MkBabble(structure R : RANDOM210
                 structure KS : KGRAM_STATS
                 structure Util : SEQUENCE_UTIL
                 sharing KS.Seq = Util.Seq
                 sharing KS.Seq = R.Seq) : BABBLE =
struct
  structure Rand = R
  structure Stats = KS
  open Stats.Seq
  structure StSeq:ST_SEQUENCE = MkSTSequence(structure Seq = Util.Seq)
  exception NoData

  (* Remove this line when you're done. *)
  exception NotYetImplemented
  fun iterih f b s = (fn (body, tail)=>append(drop(body,1),singleton tail)) (iterh f b s)
  fun randomSentence (stats : KS.kgramstats) (n : int) (seed : R.rand) =
      let 
        val maxK = KS.maxK stats 
        fun next gram = if length gram = maxK+1 then drop (gram,1) else gram
        val randomSeq = Rand.randomRealSeq seed (SOME(0.0,1.0)) n
        val hehehe = map (print o (fn x=>x^"+") o Real.toString) randomSeq
        fun operate ((gram,app), randnum) =
          case KS.lookupExts stats (next gram) of tmpHist=>
            let 
              val app = Util.choose tmpHist randnum
            in
              (append (gram, singleton app), app)
            end
        val tmp = iterih operate (empty(), "") randomSeq
      in
        (*"hasjfdhasfkld"*)
        ((String.concatWith " " o toList o map (fn (_, s)=>s)) tmp)^"."
      end

  fun randomDocument (stats : KS.kgramstats) (n : int) (seed : R.rand) =
    let
      val randomSeq = Rand.randomIntSeq seed (SOME(5, 11)) 2
      val s5 = (fn len => randomSentence stats len seed) 5;
      val end_of_s5 = print "\nend of s5\n"
      val s10 = (fn len => randomSentence stats len seed) 10;
      val end_of_s10 = print "\nend of s10\n"
      (*val tmp2 = map  (fn len => randomSentence stats len seed) randomSeq*)
      (*val tmp = toList tmp2*)
      val hehehe = map (print o (fn x=>x^"+") o Int.toString) randomSeq
    in
      (*(String.concatWith " " o toList o map (fn len => randomSentence stats len seed)) randomSeq*)
      "jsadfkjldafkjkfas"
    end
end
