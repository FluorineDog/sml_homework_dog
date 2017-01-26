structure Tests =
struct

  (* Do not remove the following line! *)
  val corpus = TextIO.inputAll (TextIO.openIn "corpus.txt")

  val testsChoose : (((string * int) list) * real) list  = [
    ([("test", 10)], 0.5),
    ([("test", 2), ("awesome", 2)], 0.5),
    ([("test", 2), ("awesome", 2)], 0.0),
    ([("test", 2), ("awesome", 2)], 0.49),
    ([("test", 2), ("awesome", 2)], 0.51),
    ([("test", 2), ("awesome", 2)], 1.0),
    ([("test", 2), ("awesome", 2)], 0.5),
    ([("yay", 1), ("woah", 2), ("oh", 3), ("yup", 4)], 0.47),
    ([("yay", 1), ("woah", 2), ("oh", 3), ("yup", 4)], 0.6),
    ([("yay", 1), ("woah", 2), ("oh", 3), ("yup", 4)], 0.1),
    ([("yay", 1), ("woah", 2), ("oh", 3), ("yup", 4)], 0.3),
    ([("yay", 1), ("woah", 2), ("oh", 3), ("yup", 4)], 0.47)
  ]

  (* You must add test kgrams for the corpus in corpus.txt as part of task 5.5
   * You may edit corpus.txt -- it will be handed in.
   *
   * You may also add other tests, which use other corpi (corpuses?), but those
   * corpuses will not be submitted. *)
  val testsKGramStats : ((string * int) * (string list)) list = [
    ((corpus, 5),
        ["",
          "direction",
         "time",
         "direction of time",
         "would write",
         "What Eddington says about"])
  ]


end
