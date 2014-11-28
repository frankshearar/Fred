namespace Fred.RegularExpression
open Fred.Regex
open System.Text

// RegularParser provides a C# friendly wrapper around Regex.
// At the moment it only supplies string-processing parsers.
type RegularParser<'a when 'a: comparison>(parser: Parser<'a>) =
    static member Empty() =
        new RegularParser<'a>(Empty)
    static member Eps() =
        new RegularParser<'a>(Eps)
    static member Token(o: 'a) =
        new RegularParser<'a>(Char o)
    static member Class(tokens: System.Collections.Generic.IEnumerable<'a>) =
        let chars = tokens
                    |> List.ofSeq
        new RegularParser<'a>(any chars)
    member this.Parser =
        parser
    member x.AtLeast(n: int) =
        new RegularParser<'a>(atLeast n x.Parser)
    member x.AtMost(n: int) =
        new RegularParser<'a>(atMost n x.Parser)
    member x.Count(n: int) =
        new RegularParser<'a>(rep n x.Parser)
    member x.Count(min: int, max: int): RegularParser<'a> =
        new RegularParser<'a>(reps min max x.Parser)
    member x.Match(s: System.Collections.Generic.IEnumerable<'a>) =
        //let toList (l: 'b list): System.Co
        s
        |> List.ofSeq
        |> find parser
        |> Seq.map (fun list -> Seq.ofList list)
    member x.Or(p: RegularParser<'a>) =
        new RegularParser<'a>(Union(parser, p.Parser))
    member x.Recognise(s: System.Collections.Generic.IEnumerable<'a>): System.Boolean =
        matchSeq x.Parser s
    member x.Then(p: RegularParser<'a>) =
        new RegularParser<'a>(Cat (parser,p.Parser))
    member x.Star() =
        new RegularParser<'a>(Star(parser))

type RegularParser =
    inherit RegularParser<System.Char>
    static member Alpha with get() = new RegularParser<char>(alpha)
    static member AlphaNum with get() = new RegularParser<char>(alphanum)
    static member Num with get() = new RegularParser<char>(num)
    member x.MatchString(s: System.String) =
        // This complicated beast turns a String into
        // Regex-friendly types, does the _one line of actual work_
        // and then converts the output back to String.
        let toString input =
            let out = new StringBuilder()
            input
            |> Array.ofList
            |> out.Append
            |> ignore
            out.ToString()
        s.ToCharArray()
        |> List.ofArray
        |> find x.Parser
        |> Seq.map (fun out -> toString out)