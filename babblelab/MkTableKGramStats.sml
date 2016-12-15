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
  type kgramstats = (token hist) T.table * int

  fun makeStats (corpus : string) (maxK : int) : kgramstats =
    let 
      val null = T.fromSeq (empty())
      val tokenlist = tokens (not o Char.isAlphaNum) corpus
      val size = length tokenlist
      fun extract k beg = (subseq tokenlist (beg, k), nth tokenlist (beg+k))
      fun construct (k:int) = tabulate (extract k) (size - k)
      val final_seq = flatten (tabulate construct (1+maxK))
    in
      (Table.map (histogram String.compare) (Table.collect final_seq), maxK)
    end

  fun lookupExts (stats : kgramstats) (kgram : kgram): token hist =
    Option.getOpt (T.find (#1 stats) kgram, empty())

  fun maxK (stats : kgramstats) : int =
    #2 stats
end
