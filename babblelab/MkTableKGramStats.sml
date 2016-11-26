functor MkTableKGramStats(structure Util : SEQUENCE_UTIL
                          structure T : TABLE
                            where type Key.t = string Util.Seq.seq
                          sharing T.Seq = Util.Seq) : KGRAM_STATS =
struct
  structure Table = T
  structure Seq = T.Seq
  open Util
  open Seq

  type token = string
  type kgram = token seq

  (* Remove this line when you're done. *)
  exception NotYetImplemented

  (* You must define the abstract kgramstats type *)
  type kgramstats = unit

  fun makeStats (corpus : string) (maxK : int) : kgramstats =
      raise NotYetImplemented

  fun lookupExts (stats : kgramstats) (kgram : kgram) : (token * int) seq =
      raise NotYetImplemented

  fun maxK (stats : kgramstats) : int =
      raise NotYetImplemented

end
