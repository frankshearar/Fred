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
    member this.Parser =
        parser
    member x.Match(s: System.String) =
        // This complicated beast a String into
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
    member x.Then(p: RegularParser) =
        new RegularParser(Cat (parser,p.Parser))
    member x.Star() =
        new RegularParser(Star(parser))