namespace Fred

module Sequences =
    // Given two sequences and a function, return the result of applying
    // that function to every pair of values in the sequences.
    // Think of Scheme's for/set.
    let allPairs xs ys f =
        match Seq.isEmpty xs, Seq.isEmpty ys with
        | true, true -> Seq.empty
        | true, false -> ys
        | false, true -> xs
        | false, false -> seq {
                               for x in xs do
                                   for y in ys do
                                       yield f x y}

    // interleave returns a sequence that draws elements from each of the sequences in turn.
    // As each sequence empties, interleave forgets about the sequence.
    // For instance interleave [Seq.ofList [1;2;3]; Seq.ofList [4;5;6]; Seq.empty; Seq.ofList [10;11]
    // returns, in order, [1;4;10;2;5;11;3;6].
    // O(n^2)! Likely because of that List.append...
    let rec interleave = function
        | [] -> Seq.empty
        | fst::rest -> seq {
                            if Seq.isEmpty fst then
                                yield! interleave rest
                            else
                                yield Seq.head fst
                                // Rotate the list of sequences, so we round-robin.
                                yield! interleave (List.append rest [Seq.skip 1 fst]) }
    let interleave2 a b = interleave [a;b]

    // interleaveSeq returns a sequence that draws elements from each of the sequences in turn.
    // As each sequence empties, interleave forgets about the sequence.
    // O(n), but doesn't interleave fairly!
    let rec interleaveSeq seqs =
        seq {
             for s in seqs do
                 if not (Seq.isEmpty s) then
                     yield Seq.head s
             let remainder = Seq.filter (fun s -> not (Seq.isEmpty s)) seqs
             if not (Seq.isEmpty remainder) then
                 yield! interleaveSeq (Seq.map (Seq.skip 1) remainder)}

    let (|LT|EQ|GT|) (a, b) =
        match compare a b with
        | 0 -> EQ
        | n when n < 0 -> LT
        | _ -> GT

    // |/ merges two ordered (according to the elements' compare) sequences
    // such that resulting sequence is ordered (by the elements' compare).
    // (I'd use (\/) like the original paper (McIlroy, Enumerating the Strings
    // of Regular Languages), but no \s allowed!
    let rec (|/) xs ys =
         match Seq.isEmpty xs, Seq.isEmpty ys with
         | true, true -> Seq.empty
         | true, false -> ys
         | false, true -> xs
         | false, false ->
            // Pulling out the tails in let bindings looks neater,
            // but means we throw one of those tails away every
            // time the comparison of heads is not EQ. |/ is used
            // A LOT, so let's be efficient with some loss of beauty.
            let x = Seq.head xs
            let y = Seq.head ys
            match x, y with
            | LT -> seq { yield x; yield! (Seq.skip 1 xs) |/ ys              }
            | EQ -> seq { yield x; yield! (Seq.skip 1 xs) |/ (Seq.skip 1 ys) }
            | GT -> seq { yield y; yield!              xs |/ (Seq.skip 1 ys) }

    // exactlyEqual returns true if the only value that sequence yields - and only once - is value.
    // It's like value = Seq.exactlyOne seq, only doesn't throw an exception.
    let exactlyEqual sequence value =
        match Seq.isEmpty sequence with
            | true  -> false
            | false ->
                let v = Seq.head sequence
                let s' = Seq.skip 1 sequence
                match Seq.isEmpty s' with
                | false -> false
                | true  -> v = value
