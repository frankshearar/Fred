module ContextFree.Test.Printing

open Fred
open NUnit.Framework
open ContextFree
open System

//    let u (f: ('a -> 'b) -> 'a -> 'b) (x: 'a): ('a -> 'b) = (f f) x

let rec fix f x = f (fix f) x
let rec fix2 f x y = f (fix f) x y

let rec fix' f = fun x -> f (fix f) x

let rec fix'' = fun f -> fun x -> f (fix f) x

//    let fix f x =
//        let mutable fix' = Unchecked.defaultof<_>
//        fix' <- fun f -> f (fix' f)
//        f (fix' f) x

let wrap = fun f -> fun f' -> fun p ->
    let result = f f' p in
    printfn "%A" result
    result

let memo =
    fun f ->
        let cache = ref Map.empty<'a, 'b>
        fun f' ->
            fun p ->
                match Map.tryFind p !cache with
                | Some v -> v
                | None ->
                    let result = f f' p
                    cache := Map.add p result !cache
                    result

let rec fact i =
    if i = 0 then 1
    else i * fact (i - 1)

let fact_tr i =
    let rec fact' i acc =
        if i = 0 then acc
        else fact' (i - 1) (acc * i)
    fact' i 1

let fact_open recur i =
    if i = 0 then 1
    else i * recur (i - 1)

type Comp<'a> =
    | Pure of 'a
    | Lambda of (unit -> Comp<'a>)
let unresolved = function
    | Pure _ -> false
    | Lambda _ -> true
let next = function
    | Pure v -> Pure v
    | Lambda l -> l()

let execute (f: 'a -> Comp<'a>) (v: 'a): 'a =
    let mutable current = Lambda <| fun () -> f v
    while unresolved current do
        current <- next current
    match current with
    | Pure v -> v
    | Lambda _ -> failwith "diverged"

let fact_cont (i: int): Comp<int> =
    let rec fact_cont' i acc =
        if i = 0 then Pure acc
        else Lambda <| fun () -> fact_cont' (i - 1) (acc * i)
    fact_cont' i 1

let fact_cont_open (i: int): Comp<int> =
    let fact_cont' recur i acc =
//            printfn "fact_cont_open ' %A %A" i acc
        if i = 0 then Pure acc
        else Lambda <| fun () -> recur (i - 1) (acc * i)
    (fix fact_cont') i 1

let fact_fix = fix (fact_open)

let mem_fact = fix (memo (fact_open))

let time f =
    let t = new System.Diagnostics.Stopwatch()
    t.Start()
    f()
    printfn "Took %d ms" t.ElapsedMilliseconds

[<TestFixture>]
type ``Cont``() =
    [<Test>]
    member __.what() =
        time <| fun () -> printfn "fact %d           = %A" 5 (fact 5)
        time <| fun () -> printfn "fact_tr %d        = %A" 5 (fact_tr 5)
        time <| fun () -> printfn "fact_fix %d       = %A" 5 (fact_fix 5)
        time <| fun () -> printfn "mem_fact %d       = %A" 5 (mem_fact 5)
        time <| fun () -> printfn "fact_cont %d      = %A" 5 (execute fact_cont 5)
        time <| fun () -> printfn "fact_cont_open %d = %A" 5 (execute fact_cont_open 5)
    [<Test>]
    member __.toComp() =
        printfn "fact 5 = %A" (execute fact_cont 5)

[<TestFixture>]
type ``Printing``() =
    [<Test>]
    member __.``Empty``() =
        let s = ContextFree.toDot (empty())
        printfn "%s" s
        let actualLines = s.Split([|Environment.NewLine|], StringSplitOptions.None)
        let expectedLines = [|
            "digraph {"
            "    0 [label=\"Empty\"]"
            "}"
            ""
        |]
        CollectionAssert.AreEqual(expectedLines, actualLines)

    [<Test>]
    member __.``Eps``() =
        let s = ContextFree.toDot (eps())
        printfn "%s" s
        let actualLines = s.Split([|Environment.NewLine|], StringSplitOptions.None)
        let expectedLines = [|
            "digraph {"
            "    0 [label=\"Eps\"]"
            "}"
            ""
        |]
        CollectionAssert.AreEqual(expectedLines, actualLines)

    [<Test>]
    member __.``Eps'``() =
        let s = ContextFree.toDot (eps'(Set.empty))
        printfn "%s" s
        let actualLines = s.Split([|Environment.NewLine|], StringSplitOptions.None)
        let expectedLines = [|
            "digraph {"
            "    0 [label=\"Eps'\"]"
            "}"
            ""
        |]
        CollectionAssert.AreEqual(expectedLines, actualLines)

    [<Test>]
    member __.``Cat``() =
        let s = ContextFree.toDot (cat (char 1) (char 2))
        printfn "%s" s
        let actualLines = s.Split([|Environment.NewLine|], StringSplitOptions.None)
        let expectedLines = [|
            "digraph {"
            "    0 [label=\"Char(1)\"]"
            "    1 [label=\"Char(2)\"]"
            "    2 [label=\"Cat\"]"
            "    2 -> 0 [label=\"first\"]"
            "    2 -> 1 [label=\"second\"]"
            "}"
            ""
        |]
        CollectionAssert.AreEqual(expectedLines, actualLines)

    [<Test>]
    member __.``Char``() =
        let s = ContextFree.toDot (char(1))
        printfn "%s" s
        let actualLines = s.Split([|Environment.NewLine|], StringSplitOptions.None)
        let expectedLines = [|
            "digraph {"
            "    0 [label=\"Char(1)\"]"
            "}"
            ""
        |]
        CollectionAssert.AreEqual(expectedLines, actualLines)

    [<Test>]
    member __.``Plus``() =
        let p = (plus (char 1))
        let s = ContextFree.toDot p
        printfn "%s" s
        let actualLines = s.Split([|Environment.NewLine|], StringSplitOptions.None)
        let expectedLines = [|
            "digraph {"
            "    0 [label=\"Char(1)\"]"
            "    1 [label=\"Plus\"]"
            "    1 -> 0 [label=\"+\"]"
            "}"
            ""
        |]
        CollectionAssert.AreEqual(expectedLines, actualLines)

    [<Test>]
    member __.``Red``() =
        let p = (red (char 1) (Func "id"))
        let s = ContextFree.toDot p
        printfn "%s" s
        let actualLines = s.Split([|Environment.NewLine|], StringSplitOptions.None)
        let expectedLines = [|
            "digraph {"
            "    0 [label=\"Char(1)\"]"
            "    1 [label=\"Red\"]"
            "    1 -> 0 [label=\"f: Int32 -> Int32\"]"
            "}"
            ""
        |]
        CollectionAssert.AreEqual(expectedLines, actualLines)

    [<Test>]
    member __.``Star``() =
        let p = (rep (char 1))
        let s = ContextFree.toDot p
        printfn "%s" s
        let actualLines = s.Split([|Environment.NewLine|], StringSplitOptions.None)
        let expectedLines = [|
            "digraph {"
            "    0 [label=\"Char(1)\"]"
            "    1 [label=\"Star\"]"
            "    1 -> 0 [label=\"*\"]"
            "}"
            ""
        |]
        CollectionAssert.AreEqual(expectedLines, actualLines)

    [<Test>]
    member __.``Union``() =
        let s = ContextFree.toDot (union (char 1) (char 2))
        printfn "%s" s
        let actualLines = s.Split([|Environment.NewLine|], StringSplitOptions.None)
        let expectedLines = [|
            "digraph {"
            "    0 [label=\"Char(1)\"]"
            "    1 [label=\"Char(2)\"]"
            "    2 [label=\"Union\"]"
            "    2 -> 0 [label=\"left\"]"
            "    2 -> 1 [label=\"right\"]"
            "}"
            ""
        |]
        CollectionAssert.AreEqual(expectedLines, actualLines)
