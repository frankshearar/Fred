namespace Fred.RegularExpression
open Fred.Regex
open System.Text

// RegularParser provides a C# friendly wrapper around Regex.
// At the moment it only supplies string-processing parsers.
type RegularParser(parser: Parser<'a>) =
    static member Empty() =
        new RegularParser(Empty)
    static member Eps() =
        new RegularParser(Eps)
    static member Token(o: System.IComparable) =
        new RegularParser(Char o)
    static member Alpha with get() = let chars = seq { yield! seq {'A'..'Z'}
                                                       yield! seq {'a'..'z'}}
                                     RegularParser.Class chars
    static member Num with get() = RegularParser.Class (seq {'0'..'9'})
    static member Class(tokens: System.Collections.Generic.IEnumerable<System.Char>) =
        let chars = tokens
                    |> Seq.map (fun c -> c :> System.IComparable)
                    |> List.ofSeq
        new RegularParser(any chars)
    member this.Parser =
        parser
    member x.AtLeast(n: int) =
        new RegularParser(atLeast n x.Parser)
    member x.AtMost(n: int) =
        new RegularParser(atMost n x.Parser)
    member x.Count(n: int) =
        new RegularParser(rep n x.Parser)
    member x.Count(min: int, max: int): RegularParser =
        new RegularParser(reps min max x.Parser)
    member x.Match(s: System.String) =
        // This complicated beast turns a String into
        // Regex-friendly types, does the _one line of actual work_
        // and then converts the output back to String.
        let toString input =
            let out = new StringBuilder()
            input
            |> List.map (fun (x: System.IComparable) -> x :?> System.Char)
            |> Array.ofList
            |> out.Append
            |> ignore
            out.ToString()
        s.ToCharArray()
        |> Array.map (fun c -> c :> System.IComparable)
        |> List.ofArray
        |> find parser
        |> Seq.map (fun out -> toString out)
    member x.Or(p: RegularParser) =
        new RegularParser(Union(parser, p.Parser))
    member x.Recognise(s: System.String) =
        s.ToCharArray()
        |> Array.map (fun c -> c :> System.IComparable)
        |> matchSeq x.Parser
    member x.Then(p: RegularParser) =
        new RegularParser(Cat (parser,p.Parser))
    member x.Star() =
        new RegularParser(Star(parser))