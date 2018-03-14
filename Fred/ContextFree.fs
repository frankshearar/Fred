namespace Fred

// NOTE WELL: At the moment this is _very much_ a work in progress. Stuff _does not work_.

module ContextFree =
    open Hekate
    open System
    open System.Text

    // The kinds of reductions we allow on parses. A discriminated union
    // because arrow types ('a -> 'b) aren't comparable.
    type Reduction<'a, 'b> =
        | Id

    let id(): Reduction<'a,'a> = Id

    type ParserType<'a , 'b> when 'a: comparison and 'b: comparison =
        | Empty                    // The non-matching parser. The error state.
        | Eps                      // Match the empty string.
        | Eps' of Set<'a list>     // Match the empty string, and contain a partial parse tree. Only appears mid-parse.
        | Char of 'a               // Recognise a single "char" (really a single token/object)
        | Union                    // Option parser: this-or-that
        | Cat                      // Sequence parser: this-then-that
        | Red of Reduction<'a, 'b> // Reduction parser: performs a function on all contained parse trees.
        | Ref                      // Reference to another parser (for creating cycles)
        | Plus                     // Optimised parser for at-least-one
        | Star                     // Kleene star parser: zero-or-more

    type GraphIndex = int
    type Parser<'a , 'b> when 'a: comparison and 'b: comparison
        (graph: Graph<GraphIndex, ParserType<'a, 'b>, string>, index: GraphIndex) =
        override __.ToString() = sprintf "Parser<%s, %s>" (typeof<'a>.Name) (typeof<'b>.Name)
        member internal __.Graph with get() = graph
        member internal __.Index with get() = index
        member internal __.Kind with get() = snd __.Node
        member internal __.Node with get() = Graph.Nodes.find index graph
        interface IComparable with
            member x.CompareTo(o: obj) =
                match o with
                | :? Parser<'a, 'b> as y -> compare (x.Graph, x.Index) (y.Graph, y.Index)
                | _ -> -1
        override x.Equals(y: obj) = (x :> IComparable).CompareTo(y) = 0
        override x.GetHashCode() = x.Graph.GetHashCode() ^^^ x.Index.GetHashCode() ^^^ x.Kind.GetHashCode()

    let private currentIndex = ref 0
    // nextIndex returns A UNIQUE index for any node in a parser graph
    let private nextIndex(): GraphIndex =
        let result = !currentIndex
        currentIndex := result + 1
        result

    let private addSingleNode parserType label (p: Parser<'a,'b>) =
        let newIndex = nextIndex()
        let graph =
            p.Graph
            |> Graph.Nodes.add (newIndex, parserType)
            |> Graph.Edges.add (newIndex, p.Index, label)
        new Parser<_,_>(graph, newIndex)

    let private parserOfType typ =
        let index = nextIndex()
        new Parser<_,_>(Graph.empty |> Graph.Nodes.add (index, typ), index)

    let cat (a: Parser<_,_>) (b: Parser<_,_>) =
        let aIndex = nextIndex()
        let bIndex = nextIndex()
        let catIndex = nextIndex()
        let graph =
            a.Graph
            |> Graph.Nodes.addMany (Graph.Nodes.toList b.Graph)
            |> Graph.Edges.addMany (Graph.Edges.toList b.Graph)
            |> Graph.Nodes.add     (catIndex, Cat)
            |> Graph.Edges.addMany [(catIndex, a.Index, "first"); (catIndex, b.Index, "second")]
        new Parser<_,_>(graph, catIndex)

    let char (x: 'a): Parser<'a,'a> =
        parserOfType <| Char x

    let empty() =
        parserOfType Empty

    let eps() =
        parserOfType Eps

    let eps' parseTrees =
        parserOfType <| Eps' parseTrees

    let plus (p: Parser<_,_>) =
        addSingleNode Plus "+" p

    let red (p: Parser<'a,'b>) (f: Reduction<'a,'b>) =
        addSingleNode (Red f) (sprintf "f: %s -> %s" typeof<'a>.Name typeof<'b>.Name) p

    // Return a parser that will, eventually, point to another parser.
    let refer() =
        let newIndex = nextIndex()
        let graph =
            Graph.empty
            |> Graph.Nodes.add (newIndex, Ref)
        new Parser<_,_>(graph, newIndex)

    // Given a Ref parser within a parser, make that Ref actually reference 
    let stitch (referer: Parser<'a,'b>) (reference: Parser<'a,'b>) (p: Parser<'a,'b>) =
        let startOfArrow = Graph.Nodes.find (referer.Index) p.Graph
        let endOfArrow = Graph.Nodes.find (reference.Index) p.Graph
        let graph =
            p.Graph
            |> Graph.Edges.add ((fst startOfArrow), (fst endOfArrow), "=>")
        new Parser<_,_>(graph, p.Index)

    let rep (p: Parser<_,_>) =
        addSingleNode Star "*" p

    let star = rep

    let union (a: Parser<_,_>) (b: Parser<_,_>) =
        let unionIndex = nextIndex()
        let graph =
            a.Graph
            |> Graph.Nodes.addMany (Graph.Nodes.toList b.Graph)
            |> Graph.Edges.addMany (Graph.Edges.toList b.Graph)
            |> Graph.Nodes.add     (unionIndex, Union)
            |> Graph.Edges.addMany [(unionIndex, a.Index, "left"); (unionIndex, b.Index, "right")]
        new Parser<_,_>(graph, unionIndex)

    let toDot (p: Parser<'a, 'b>) =
        let parserTypeToStr = function
            | Empty  -> "Empty"
            | Eps    -> "Eps"
            | Eps' _ -> "Eps'"
            | Char x -> sprintf "Char(%A)" x
            | Union  -> "Union"
            | Cat    -> "Cat"
            | Red _  -> "Red"
            | Ref    -> "Ref"
            | Plus   -> "Plus"
            | Star   -> "Star"
        let mutable mark = 0
        let marks = new System.Collections.Generic.Dictionary<_,_>()
        let builder = new StringBuilder()
        builder.AppendLine "digraph {" |> ignore
        Graph.Nodes.toList p.Graph
        |> List.map (fun (index, parserType) ->
            let result = sprintf "    %A [label=\"%s\"]" mark (parserTypeToStr parserType)
            marks.Add(index, mark)
            mark <- mark + 1
            result)
        |> List.fold (fun (builder: StringBuilder) nodeString -> builder.AppendLine nodeString) builder
        |> ignore
        marks |> Seq.iter (fun kvp -> printfn ">>> %A -> %A" kvp.Key kvp.Value)
        Graph.Edges.toList p.Graph
        |> List.map (fun (startNode, endNode, label) ->
            let s = marks.[startNode]
            let e = marks.[endNode]
            sprintf "    %A -> %A [label=\"%s\"]" marks.[startNode] marks.[endNode] label)
        |> List.fold (fun (builder: StringBuilder) nodeString -> builder.AppendLine nodeString) builder
        |> ignore
        builder.AppendLine "}" |> ignore
        builder.ToString()

    let left (p: Parser<_,_>) =
        match p.Kind with
        | Union ->
            match Graph.Nodes.successors p.Index (p.Graph) with
            | Some (x::_::_) -> new Parser<_,_>(p.Graph, fst x)
            | Some [_] -> failwith "Found a Union node with only one out edge!"
            | Some [] -> failwith "Found a Union node with no out edges!"
            | None -> failwith "Could not find the Union node in the graph!"
        | _ -> invalidArg "p" (sprintf "You can only call left on a Union, not a %A" p.Kind)

    let right (p: Parser<_,_>) =
        match p.Kind with
        | Union ->
            match Graph.Nodes.successors p.Index (p.Graph) with
            | Some (_::x::_) -> new Parser<_,_>(p.Graph, fst x)
            | Some [_] -> failwith "Found a Union node with only one out edge!"
            | Some [] -> failwith "Found a Union node with no out edges!"
            | None -> failwith "Could not find the Union node in the graph!"
        | _ -> invalidArg "p" (sprintf "You can only call right on a Union, not a %A" p.Kind)

    let first (p: Parser<_,_>) =
        match p.Kind with
        | Cat ->
            match Graph.Nodes.successors p.Index (p.Graph) with
            | Some (x::_::_) -> new Parser<_,_>(p.Graph, fst x)
            | Some [_] -> failwith "Found a Cat node with only one out edge!"
            | Some [] -> failwith "Found a Cat node with no out edges!"
            | None -> failwith "Could not find the Cat node in the graph!"
        | _ -> invalidArg "p" (sprintf "You can only call first on a Cat, not a %A" p.Kind)

    let second (p: Parser<_,_>) =
        match p.Kind with
        | Cat ->
            match Graph.Nodes.successors p.Index (p.Graph) with
            | Some (_::x::_) -> new Parser<_,_>(p.Graph, fst x)
            | Some [_] -> failwith "Found a Cat node with only one out edge!"
            | Some [] -> failwith "Found a Cat node with no out edges!"
            | None -> failwith "Could not find the Cat node in the graph!"
        | _ -> invalidArg "p" (sprintf "You can only call second on a Cat, not a %A" p.Kind)

    let inner (p: Parser<_,_>) =
        match p.Kind with
        | Red _
        | Ref
        | Plus
        | Star ->
            match Graph.Nodes.successors p.Index (p.Graph) with
            | Some [x] -> new Parser<_,_>(p.Graph, fst x)
            | Some (_::_) -> failwith (sprintf "Found a %A node with more than one out edge!" p.Kind)
            | Some [] -> failwith (sprintf "Found a %A node with no out edges!" p.Kind)
            | None -> failwith (sprintf "Could not find the %A node in the graph!" p.Kind)
        | _ -> invalidArg "p" (sprintf "You can only call inner on a Red, Ref, Plus or Star, not a %A" p.Kind)

    let memoize = fun f -> fun f' -> fun p ->
        let cache = ref Map.empty<int, int>
        match Map.tryFind p !cache with
        | Some v -> v
        | None ->
            let result = f f' p
            cache := Map.add p result !cache
            result

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
    open System.Threading
    let fix (body: ('a -> 'b) -> ('a -> 'b)) (bottom: 'b): ('a -> 'b) =
        let visited = new ThreadLocal<Set<'a>>()
        let cache = ref Map.empty
        let hasChanged = new System.Threading.ThreadLocal<bool>()
        let running = new ThreadLocal<bool>()
        let rec f (x: Parser<_,_>) =
            printfn "f %A" (x.Kind)
            let isCached = Map.containsKey x !cache
            let cached = match Map.tryFind x !cache with
                         | Some a -> a
                         | None -> bottom
            let run = running.Value
            if isCached && not run then
                cached
            elif run && (Set.contains x visited.Value) then
                if isCached then cached else bottom
            elif run then
                visited.Value <- Set.add x visited.Value
                let newVal = body f x // body, 'a -> 'b, is applied with a 'b to yield an 'a
                if newVal <> cached then
                    hasChanged.Value <- true
                    cache := Map.add x newVal !cache
                newVal
            elif not isCached && not run then
                let mutable v = bottom
                hasChanged.Value <- true
                running.Value <- true
                while hasChanged.Value do
                    hasChanged.Value <- false
                    visited.Value <- Set.empty
                    v <- f x
                v
            else failwith "How'd we end up here??"
        f

    let private print f recur (x: Parser<_,_>) =
        printfn "print >>"
        let result = f recur x
        printfn "<< %A -> %A" (x.Kind) result
        result

    let private isNullable recur (p: Parser<_,_>): bool =
        printfn "isNullable %A" (p.Kind)
        match snd (Graph.Nodes.find p.Index p.Graph) with
        | Empty -> false
        | Eps -> true
        | Eps' _ -> true
        | Char _ -> false
        | Union -> (recur (left p)) || (recur (right p))
        | Cat -> (recur (first p)) && (recur (second p))
        | Red _ -> true
        | Ref
        | Plus -> recur (inner p)
        | Star -> true

    // TODO: add cache as parameter for application-level control
    // nullable answers the question "does this language accept the empty string?"
    let nullable p =
        let what = print isNullable
        fix what false p

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
//    let memoize' f (cache: Map<'a, 'b> ref) =
//        (fun x -> match Map.tryFind x (!cache) with
//                  | Some y -> y
//                  | None -> let y = f x
//                            cache := Map.add x y !cache
//                            y)
//
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

//    // mapPrim takes a structure and a function defining how to extract the children of a node
//    // in that structure, and walks the structure in a cycle-safe manner, returning a Seq of
//    // the actions of some function on each node. This _does not_ return a structure isomorphic
//    // to the original.
//    let mapPrim f getChildren p =
//        let rec map' f p visited =
//            if Set.contains p visited then
//                Seq.ofList []
//            else
//                let this = Seq.ofList [f p]
//                let newVisited = Set.add p visited
//                let mappedChildren = Seq.concat (Seq.map (fun each -> map' f each newVisited) (getChildren p))
//                Seq.append this mappedChildren
//        map' f p Set.empty


//    // nullable answers the question "does this language accept the empty string?"
//    let rec is_nullable = function
//        | Empty -> false
//        | Eps _ -> true
//        | Char _ -> false
//        | Union (a, b) -> is_nullable a || is_nullable b
//        | Cat (a, b) -> is_nullable a && is_nullable b
////        | Rep a -> true
//        | Ref a -> is_nullable !a
//
//    // TODO: add cache as parameter for application-level control
//    let rec nullable p = fix is_nullable false p
//
//    // TODO: fix, fixpointise
//    let nil _ = Eps 0
//
//    // "char" really means "the next thing in the stream, whatever it may be"
//    let rec derive expected parser =
//        match parser with
//        | Empty -> Empty
//        | Eps _ -> Empty
//        | Char c when c = expected -> Eps c
//        | Char _ -> Empty
//        | Union (a, b) -> Union (derive expected a, derive expected b)
//        | Cat (a, b) -> Union (Cat (derive expected a, b), Cat (nil a, derive expected b))
////        | Rep a -> Cat (derive expected a, Rep a)
//        | Ref a -> Ref (ref (derive expected !a))
//
//    let rec D expected = function
//        | Empty -> Empty
//        | Eps _ -> Empty
//        | Char c when c = expected -> Eps c
//        | Char _ -> Empty
//        | Union (a, b) -> Union (D expected a, D expected b)
//        | Cat (a, b) when nullable a -> b
//        | Cat (a, b) -> Cat (D expected a, b)
////        | Rep a -> Cat (D expected a, Rep a)
//        | Ref a -> Ref (ref (D expected !a))
//
//    let rec parseNull = function
//        | Empty -> []
//        | Eps t -> [t]
//        | Char _ -> []
//        | Union (a, b) -> List.append (parseNull a) (parseNull b)
//        | Cat (a, b) ->  parseNull a // TODO: totally wrong!
//        | Ref a -> parseNull !a
//
//    let rec parse input p =
//        match input with
//        | x::xs -> D x p |> parse xs
//        | [] -> parseNull p
//
//    let map f p =
//        let cache = new System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
//        let rec map' f p =
//            if not (cache.ContainsKey p) then cache.[p] <- f p
//            let this = Seq.ofList [cache.[p]]
//            match p with
//            | Union (a, b) ->
//                Seq.concat [this; (map' f a); (map' f b)]
//            | Cat (a, b) ->
//                Seq.concat [this; (map' f a); (map' f b)]
//            | Ref x ->
//                Seq.append (this) (map' f !x)
//            | x ->
//                this
//        map' f p
//
//    let rec fix1 f x = f (fix1 f) x
//    // Even if I can make this work, the parsers are cyclic (because of Refs), and MUST memoise and other funky stuff
//    let rec fix2 f x y = f (fix2 f) x y
//
//    let fact' f = function
//        | 0 -> 1
//        | x -> x * f (x - 1)
//    let fact = fix1 fact'
//
//    fact 5
//
//    let size p =
//        let rec size' f i = function
//            | Cat (a, b) -> 1 + (f i a) + (f i b)
//            | Union (a, b) -> 1 + (f i a) + (f i b)
//            | Ref a -> 1 + (f i !a)
//            | _ -> i + 1
//        (fix2 size') 0 p
//
//    // print' returns a string containing a dotfile representation of a parser, _given a cache_.
//    let print' (p: Parser<_,_>) (cache: System.Collections.Generic.Dictionary<_,int>): string =
//        let sb = new System.Text.StringBuilder()
//        let max = ref 0
//        let markOfBeast p: int =
//            let next = fun () ->
//                max := !max + 1
//                !max
//            if cache.ContainsKey(p) then
//                cache.[p]
//            else
//                let i = next()
//                cache.[p] <- i
//                i
//        let dotifyNode p (s: System.Text.StringBuilder) index: unit =
//            map (fun x ->
//                let mark = markOfBeast x
//                match x with
//                | Empty ->
//                    Printf.bprintf s "  %d [label=\"empty\"]\n" mark
//                // Eps*
//                | Eps t ->
//                    Printf.bprintf s "  %d [shape=\"record\", label=\"eps\"]\n" mark
//                | Char c ->
//                    Printf.bprintf s "  %d [shape=\"record\", label=\"%s\"]\n" mark c
//                | Union (a,b) ->
//                    let ma = markOfBeast a
//                    let mb = markOfBeast b
//                    Printf.bprintf s "  %d [label=\"or\"]\n" mark
//                    Printf.bprintf s "  %d -> %d\n" mark ma
//                    Printf.bprintf s "  %d -> %d\n" mark mb
//                | Cat (a,b) ->
//                    let ma = markOfBeast a
//                    let mb = markOfBeast b
//                    Printf.bprintf s "  %d [shape=\"none\", margin=0, label=<\n<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" cellpadding=\"4\"><tr><td colspan=\"2\">seq</td></tr><tr><td port=\"L\">L</td><td port=\"R\">R</td></tr></table>>]\n" mark |> ignore
//                    Printf.bprintf s "   %d:L -> %d\n" mark ma
//                    Printf.bprintf s "   %d:R -> %d\n" mark mb
//                | Ref a ->
//                    let ma = markOfBeast !a
//                    Printf.bprintf s "  %d [label=\"ref\"]\n" mark
//                    Printf.bprintf s "  %d -> %d\n" mark ma) p |> ignore
//        Printf.bprintf sb "digraph {\n"
//        dotifyNode p sb cache
//        Printf.bprintf sb "}"
//        sb.ToString()
//
//    // print returns a string containing a dotfile representation of a parser.
//    let print p = new System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural) |> print' p
//
//    let left = function
//        | Union (a, b) -> a
//        | unknown -> failwith "Cannot left a %A" unknown
//    let right = function
//        | Union (a, b) -> b
//        | unknown -> failwith "Cannot right a %A" unknown
//
//    let first = function
//        | Cat (a, b) -> a
//        | unknown -> failwith "Cannot first on a %A" unknown
//    let second = function
//        | Cat (a, b) -> b
//        | unknown -> failwith "Cannot second on a %A" unknown
//
//    let deref = function
//        | Ref x -> !x
//        | unknown -> failwith "Cannot deref on a %A" unknown
//    let setRef parser p =
//        match p with
//            | Ref a ->
//                a := parser
//                p
//            | unknown -> failwith (sprintf "Cannot set ref of a %A" unknown)

(*
use f = new StreamWriter("foo.dot")
    f.Write(print (Cat (Empty, (Eps "a"))))
    f.Flush
*)
