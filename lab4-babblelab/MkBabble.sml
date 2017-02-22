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
  fun iterih f b s = (fn (body, tail) =>
    append(drop(body,1),singleton tail)) (iterh f b s)
  fun randomSentence 
    (stats : KS.kgramstats) (n : int) (seed : R.rand) =
      let 
        val maxK = KS.maxK stats 
        fun next gram = if length gram = maxK+1 
          then drop (gram,1) else gram
        val randomSeq = Rand.randomRealSeq seed (SOME(0.0,1.0)) n
        fun operate ((gram,app), randnum) =
          case KS.lookupExts stats (next gram) 
          of tmpHist=> if length tmpHist = 0 
            then operate((drop (gram,1),""),randnum) 
            else
              let 
                val app = Util.choose tmpHist randnum
              in
                (append (gram, singleton app), app)
              end
        val tmp = iterih operate (empty(), "") randomSeq
      in
        ((String.concatWith " " o toList o map (fn (_, s)=>s)) tmp)^"."
      end

  fun randomDocument (stats : KS.kgramstats) (n : int) (seed : R.rand) =
    let
      val lenSeq = Rand.randomIntSeq seed (SOME(5, 11)) n
      val seedSeq = map R.fromInt (Rand.randomIntSeq seed NONE n)
      val sentenceSeq = map2  
        (fn (len, seqseed) => randomSentence stats len seqseed) 
        lenSeq seedSeq
      val sentList = toList sentenceSeq
    in
      String.concatWith " " sentList
    end
end
