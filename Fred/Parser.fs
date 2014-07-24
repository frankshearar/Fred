namespace Fred

// NOTE WELL: At the moment this is _very much_ a work in progress. Stuff _does not work_.

module Cfg =
(*    let equalsOn f x (yobj:obj) =
            match yobj with
            | :? 'T as y -> (f x = f y)
            | _ -> false

    let hashOn f x =  hash (f x)

    let compareOn f x (yobj: obj) =
        match yobj with
        | :? 'T as y -> compare (f x) (f y)
        | _ -> invalidArg "yobj" "cannot compare values of different types"
*)
    let memoize' f (cache: Map<'a, 'b> ref) =
        (fun x -> match Map.tryFind x (!cache) with
                  | Some y -> y
                  | None -> let y = f x
                            cache := Map.add x y !cache
                            y)

    // This crappy memoization routine grows its cache without bound. Can you say "memory leak"?
    let memoize f = ref Map.empty |> memoize' f

    // fix uses Kleene's fixed-point theorem to calculate the fixpoint of a set of recursive equations, body:
    // Given a monotone function f, walk the chain
    //     bottom <= f(bottom) <= f(f(bottom)) <= f(f(f(bottom) <= ... f^n(bottom) <= ... <= lfp(f)
    // until we hit the least fixed point (which Kleene assures us does exist).
    // Since we have a set of mutually recursive equations, we need to track
    //   * the last f^k(x) (obviously)
    //   * the new value f^(k+1)(x)
    //   * the "nodes" in the search graph we have already visited (because of potential cycles)
    //
    // More-or-less word-for-word translation of Matt Might's define/fix, from his dparse.rkt library.
    // We use ref cells; the Racket version uses parameters. These MAY be like dynamic variables?!
    // body represents the recursive function whose fixpoint we will return.
    let rec fix body bottom =
        let visited = ref Set.empty
        let cache = ref Map.empty
        let hasChanged = ref false
        let running = ref false
        // g lets us call f and then recur _on g_. This simulates define/fix's macro insertion of body
        let rec f x =
            let isCached = Map.containsKey x (!cache)
            let cached = match Map.tryFind x !cache with
                         | Some a -> a
                         | None -> bottom
            let run = !running
            if (isCached && not run) then
                // We already know the answer
                cached
            elif (run && (Set.contains x !visited)) then
                // We've visited this node already
                if isCached then cached else bottom
            elif (run) then
                // Climb one rung up the Kleene chain
                visited := Set.add x !visited
                let newVal = body x // Here's where define/fix inserts the body into the resulting function. No macros in F#...
                if newVal <> cached then
                    hasChanged := true
                    cache := Map.add x newVal !cache
                newVal
            elif (not isCached && not run) then
                // First step of the computation: nothing's cached, and we've not run.
                // Set the scene.
                let mutable v = bottom
                while not !hasChanged do
                    hasChanged := false
                    visited := Set.empty
                    v <- f x // Recur!
                v
            else failwith "How'd we end up here??"
        f


    // mapPrim takes a structure and a function defining how to extract the children of a node
    // in that structure, and walks the structure in a cycle-safe manner, returning a Seq of
    // the actions of some function on each node. This _does not_ return a structure isomorphic
    // to the original.
    let mapPrim f getChildren p =
        let rec map' f p visited =
            if Set.contains p visited then
                Seq.ofList []
            else
                let this = Seq.ofList [f p]
                let newVisited = Set.add p visited
                let mappedChildren = Seq.concat (Seq.map (fun each -> map' f each newVisited) (getChildren p))
                Seq.append this mappedChildren
        map' f p Set.empty

    [<CustomEquality; CustomComparison>]
    type Parser<'a , 'b> when 'a: comparison and 'b: comparison =
        | Empty                                    // The empty parser
        | Eps of 'b                                // A partial parse tree
        | Char of 'a                               // Recognise a single "char" (really a single token/object)
        | Union of Parser<'a, 'b> * Parser<'a, 'b> // Option parser: this-or-that
        | Cat of Parser<'a, 'b> * Parser<'a, 'b>   // Sequence parser: this-then-that
//        | Rep of Parser<'a, 'b>                  // Kleene star parser: zero-or-more
//        | Red of Parser<'a, 'b> * ('b -> 'b)     // Reduction parser: performs a function on all contained parse trees.
        | Ref of Parser<'a, 'b> ref                // Reference to another parser (for creating cycles)
//        | Plus of Parser<'a, 'b>                 // Optimised parser for at-least-one
        interface System.IComparable with
            member x.CompareTo(yobj: obj): int =
                match yobj with
                | :? Parser<'a, 'b> as y ->
                    let merge a b =
                        if a = 0 then
                            if b = 0 then 0 else b
                        else a
                    let rec primComp (x: Parser<'a,'b>) y =
                        // This gigantic pattern match might be tediously long, but is explicit, and so safe against the
                        // addition of new parsers (in the sense that exhaustiveness checking will complain loudly).
                        // Matching against (x,y) is mildly shorter, but turns out harder to read.
                        printfn "comparing %A and %A\n" x y
                        match x with
                        | Eps a -> match y with
                                   | Eps b -> compare a b
                                   | Empty -> -1
                                   | Char _ -> -1
                                   | Union (_,_) -> -1
                                   | Cat (_,_) -> -1
                                   | Ref _ -> -1
                        | Empty -> match y with
                                   | Eps _ -> 1
                                   | Empty -> 0
                                   | Char _ -> -1
                                   | Union (_,_) -> -1
                                   | Cat(_,_) -> -1
                                   | Ref _ -> -1
                        | Char a -> match y with
                                    | Eps _ -> 1
                                    | Empty -> 1
                                    | Char b -> compare a b
                                    | Union (_,_) -> -1
                                    | Cat (_,_) -> -1
                                    | Ref _ -> -1
                        | Union (a1,b1) -> match y with
                                           | Eps _ -> 1
                                           | Empty -> 1
                                           | Char _ -> 1
                                           | Union (a2,b2) -> merge (primComp a1 a2) (primComp b1 b2)
                                           | Cat (_,_) -> -1
                                           | Ref _ -> -1
                        | Cat (a1,b1) -> match y with
                                         | Eps _ -> 1
                                         | Empty -> 1
                                         | Char _ -> 1
                                         | Union (_,_) -> 1
                                         | Cat (a2,b2) -> merge (primComp a1 a2) (primComp b1 b2)
                                         | Ref _ -> -1
                        | Ref a -> match y with
                                   | Eps _ -> 1
                                   | Empty -> 1
                                   | Char _ -> 1
                                   | Union (_,_) -> 1
                                   | Cat (_,_) -> 1
                                   // Refs can introduce cycles. "Equals" means "points to the same parser",
                                   // which we determine with reference equality. In the < and > cases, we
                                   // simply don't care. It does mean that Ref (ref x) < Ref (ref y) AND
                                   // Ref (ref y) < Ref (ref x), destroying the antisymmetric property of
                                   // the partial order...
                                   | Ref b ->
                                       let abang = !a
                                       let bbang = !b
                                       if System.Object.ReferenceEquals(abang, bbang) then 0 else -1
                    primComp x y
                | _ -> -1 // Order any parsers before non-parsers
        override x.Equals(yobj) =
            match yobj with
                | :? Parser<'a, 'b> as y ->
                    let findChildren = function
                        | Empty -> []
                        | Eps _ -> []
                        | Char _ -> []
                        | Union (a,b) -> [a;b]
                        | Cat (a,b) -> [a;b]
                        | Ref x -> [!x]
                    Seq.fold (fun acc (a,b) -> acc && (compare a b = 0)) true (Seq.zip (mapPrim id findChildren x) (mapPrim id findChildren y))
                | _ -> false
        override x.GetHashCode() =
            match x with
            | Empty -> 0
            | Eps t -> hash t
            | Char c -> hash c
            | Union (a, b) -> (19 * hash a) + hash b
            | Cat (a, b) -> (19 * hash a) + hash b
            | Ref p -> hash p // Hash the cell, deliberately. _Cannot_ use "hash !p" because that will cause a cyclic evaluation.

//    type Cache<'a, 'b> =
//        abstract member Evict: Cache<'a, 'b> -> 'a -> Cache<'a, 'b>
//        abstract member HasValue: 'a -> bool
//        abstract member LookUp: 'a -> 'b option
//        abstract member LookUpWithDefault: 'a -> 'b -> 'b
//        abstract member Hit: Cache<'a, 'b> -> 'a -> Cache<'a, 'b>
//        abstract member Miss: Cache<'a, 'b> -> 'a -> Cache<'a, 'b>
//
//    let basicCache =
//        let cache = new Dictionary<_,_>(HashIdentity.Structural)
//        { new Cache<'a, 'b> with
//            member this.HasValue(v: 'a) = cache.ContainsKey(v)
//            member this.LookUp(v: 'a) = match cache.TryGetValue(v) with
//                                        | true, y -> Some y
//                                        | _ -> None}
//

    // nullable answers the question "does this language accept the empty string?"
    let rec is_nullable = function
        | Empty -> false
        | Eps _ -> true
        | Char _ -> false
        | Union (a, b) -> is_nullable a || is_nullable b
        | Cat (a, b) -> is_nullable a && is_nullable b
//        | Rep a -> true
        | Ref a -> is_nullable !a

    // TODO: add cache as parameter for application-level control
    let rec nullable p = fix is_nullable false p

    // TODO: fix, fixpointise
    let nil _ = Eps 0

    // "char" really means "the next thing in the stream, whatever it may be"
    let rec derive expected parser =
        match parser with
        | Empty -> Empty
        | Eps _ -> Empty
        | Char c when c = expected -> Eps c
        | Char _ -> Empty
        | Union (a, b) -> Union (derive expected a, derive expected b)
        | Cat (a, b) -> Union (Cat (derive expected a, b), Cat (nil a, derive expected b))
//        | Rep a -> Cat (derive expected a, Rep a)
        | Ref a -> Ref (ref (derive expected !a))

    let rec D expected = function
        | Empty -> Empty
        | Eps _ -> Empty
        | Char c when c = expected -> Eps c
        | Char _ -> Empty
        | Union (a, b) -> Union (D expected a, D expected b)
        | Cat (a, b) when nullable a -> b
        | Cat (a, b) -> Cat (D expected a, b)
//        | Rep a -> Cat (D expected a, Rep a)
        | Ref a -> Ref (ref (D expected !a))

    let rec parseNull = function
        | Empty -> []
        | Eps t -> [t]
        | Char _ -> []
        | Union (a, b) -> List.append (parseNull a) (parseNull b)
        | Cat (a, b) ->  parseNull a // TODO: totally wrong!
        | Ref a -> parseNull !a

    let rec parse input p =
        match input with
        | x::xs -> D x p |> parse xs
        | [] -> parseNull p

    let map f p =
        let cache = new System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        let rec map' f p =
            if not (cache.ContainsKey p) then cache.[p] <- f p
            let this = Seq.ofList [cache.[p]]
            match p with
            | Union (a, b) ->
                Seq.concat [this; (map' f a); (map' f b)]
            | Cat (a, b) ->
                Seq.concat [this; (map' f a); (map' f b)]
            | Ref x ->
                Seq.append (this) (map' f !x)
            | x ->
                this
        map' f p

    let rec fix1 f x = f (fix1 f) x
    // Even if I can make this work, the parsers are cyclic (because of Refs), and MUST memoise and other funky stuff
    let rec fix2 f x y = f (fix2 f) x y

    let fact' f = function
        | 0 -> 1
        | x -> x * f (x - 1)
    let fact = fix1 fact'

    let size p =
        let rec size' f i = function
            | Cat (a, b) -> 1 + (f i a) + (f i b)
            | Union (a, b) -> 1 + (f i a) + (f i b)
            | Ref a -> 1 + (f i !a)
            | _ -> i + 1
        (fix2 size') 0 p

    // print' returns a string containing a dotfile representation of a parser, _given a cache_.
    let print' (p: Parser<_,_>) (cache: System.Collections.Generic.Dictionary<_,int>): string =
        let sb = new System.Text.StringBuilder()
        let max = ref 0
        let markOfBeast p: int =
            let next = fun () ->
                max := !max + 1
                !max
            if cache.ContainsKey(p) then
                cache.[p]
            else
                let i = next()
                cache.[p] <- i
                i
        let dotifyNode p (s: System.Text.StringBuilder) index: unit =
            map (fun x ->
                let mark = markOfBeast x
                match x with
                | Empty ->
                    Printf.bprintf s "  %d [label=\"empty\"]\n" mark
                // Eps*
                | Eps t ->
                    Printf.bprintf s "  %d [shape=\"record\", label=\"eps\"]\n" mark
                | Char c ->
                    Printf.bprintf s "  %d [shape=\"record\", label=\"%s\"]\n" mark c
                | Union (a,b) ->
                    let ma = markOfBeast a
                    let mb = markOfBeast b
                    Printf.bprintf s "  %d [label=\"or\"]\n" mark
                    Printf.bprintf s "  %d -> %d\n" mark ma
                    Printf.bprintf s "  %d -> %d\n" mark mb
                | Cat (a,b) ->
                    let ma = markOfBeast a
                    let mb = markOfBeast b
                    Printf.bprintf s "  %d [shape=\"none\", margin=0, label=<\n<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" cellpadding=\"4\"><tr><td colspan=\"2\">seq</td></tr><tr><td port=\"L\">L</td><td port=\"R\">R</td></tr></table>>]\n" mark |> ignore
                    Printf.bprintf s "   %d:L -> %d\n" mark ma
                    Printf.bprintf s "   %d:R -> %d\n" mark mb
                | Ref a ->
                    let ma = markOfBeast !a
                    Printf.bprintf s "  %d [label=\"ref\"]\n" mark
                    Printf.bprintf s "  %d -> %d\n" mark ma) p |> ignore
        Printf.bprintf sb "digraph {\n"
        dotifyNode p sb cache
        Printf.bprintf sb "}"
        sb.ToString()

    // print returns a string containing a dotfile representation of a parser.
    let print p = new System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural) |> print' p

    let left = function
        | Union (a, b) -> a
        | unknown -> failwith "Cannot left a %A" unknown
    let right = function
        | Union (a, b) -> b
        | unknown -> failwith "Cannot right a %A" unknown

    let first = function
        | Cat (a, b) -> a
        | unknown -> failwith "Cannot first on a %A" unknown
    let second = function
        | Cat (a, b) -> b
        | unknown -> failwith "Cannot second on a %A" unknown

    let deref = function
        | Ref x -> !x
        | unknown -> failwith "Cannot deref on a %A" unknown
    let setRef parser p =
        match p with
            | Ref a ->
                a := parser
                p
            | unknown -> failwith (sprintf "Cannot set ref of a %A" unknown)

(*
use f = new StreamWriter("foo.dot")
    f.Write(print (Cat (Empty, (Eps "a"))))
    f.Flush
*)

type Class1() = 
    member this.X = "F#"