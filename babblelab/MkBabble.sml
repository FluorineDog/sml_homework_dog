functor MkBabble(structure R : RANDOM210
                 structure KS : KGRAM_STATS
                 structure Util : SEQUENCE_UTIL
                 sharing KS.Seq = Util.Seq
                 sharing KS.Seq = R.Seq) : BABBLE =
struct
  structure Rand = R
  structure Stats = KS
  open Stats.Seq

  exception NoData

  (* Remove this line when you're done. *)
  exception NotYetImplemented

  fun randomSentence (stats : KS.kgramstats) (n : int) (seed : R.rand) =
    case stats of (gram, maxK) =>
    let 
    in
    ed 

  fun randomDocument (stats : KS.kgramstats) (n : int) (seed : R.rand) =
      raise NotYetImplemented

end
