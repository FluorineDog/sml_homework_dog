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
  type 'a hist = (token*int) seq

  (* Remove this line when you're done *)
  exception NotYetImplemented

  (* You must define the abstract kgramstats type *)
  type kgramstats = (token seq) T.table

  fun makeStats (corpus : string) (maxK : int) : kgramstats =
    let 
        val null = T.fromSeq (empty())
        fun construct (k:int):(token seq * token) = 
    in
       null 
    end
      (*raise NotYetImplemented*)

  fun lookupExts (stats : kgramstats) (kgram : kgram)
    : token hist = 
   (*: (token * int) seq =*)
      raise NotYetImplemented

  fun maxK (stats : kgramstats) : int =
      raise NotYetImplemented

end
