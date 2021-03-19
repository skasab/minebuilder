module ProjectParser
open System.IO
open System
open Parser
open System.Diagnostics


type Program = Command list 
and Command = 
| Build of  string * Coords
| Define of string * Blueprint
| Repeat of string * Coords * int * int * char
| End
and Coords = Coords of int * int * int
and Blueprint = Blueprint of Slice list
and Slice = Slice of Line list
and Line = Line of char list

exception NoFileFoundException of string * string
 
let blocks = ['.';'S';'R';'D';'C';'W';'s';'r';'1';'2';'3';'w';'l';'~';'4';'L';'N';'P';'G';'I';'?';'@';'B';'b';'t';'M';'O';'+';'5';'d';'$';'F';'6';'i';'^';'c';'H';'&';'*';'p';'m';'o']
let reservedKeywords = Set.ofList["define";"build";"end";"as";"at";"ERROR";"repeat";]

let parse3 p1 p2 p3 = (pseq p1 (pseq p2 p3 id) id) |>> (fun (a,(b,c)) -> (a,b,c)) <!> "parse3"
let parse4 p1 p2 p3 p4 = (pseq p1 (parse3 p2 p3 p4) id) |>> (fun (a,(b,c,d)) -> (a,b,c,d)) <!> "parse4"
let parse5 p1 p2 p3 p4 p5 = (pseq p1 (parse4 p2 p3 p4 p5) id) |>> (fun (a,(b,c,d,e)) -> (a,b,c,d,e)) <!> "parse5"

let negate x = -x
let pname = (pmany1 (pletter <|> pdigit <|> (pchar '-') <|> (pchar '_'))) |>> stringify <!> "pname"
let pdefinename = pname |>> (fun name -> if (not (reservedKeywords.Contains name)) 
                                                                    then name
                                                                    else ("ERROR" + name))

let pposnum = pmany1 pdigit |>> stringify |>> int <!> "pposnum"
let pnegnum = pright (pchar '-') pposnum |>> negate  <!> "pnegnum"
let pnum = (pposnum <|> pnegnum) <!> "pnum"
let coords = pbetween (pchar '(') (pchar ')') (pseq (pleft pnum (pchar ','))
                                              (pseq (pleft pnum (pchar ',')) pnum id)
                                              (fun (a,(b,c)) -> (a,b,c))) |>> (fun (x,y,z) -> Coords(x,y,z)) <!> "Coords"
let line = (pleft (pright (pws0) (pmany1 (psat (fun blockChar -> List.contains blockChar blocks)))) pnl) |>> Line <!> "Line"
let slice = (pleft (pmany1 line) (pleft (pleft pws0 (pstr "-")) pws0)) |>> Slice <!> "Slice"
let blueprint = (pmany1 slice) |>> Blueprint <!> "Blueprint"
let endParser = (pleft (pstr "end") pws0) |>> (fun _ -> End) <!> "End"

let build = (pright (pstr "build ") (pseq pname
                                         (pright (pstr " at ") coords) id) 
                                         |>> (fun (name,coord) -> Build(name,coord))) <!> "Build"                    
let define = (pright (pstr "define ") (pseq pdefinename 
                                         (pright (pleft (pstr " as") pws0)
                                         (pbetween (pleft (pchar '{') pws0) (pleft (pchar '}') pws0) blueprint)) id)
                                         |>> (fun (name,blueprint) -> Define(name,blueprint))) <!> "Define"

let pdir = ((pchar 'x') <|> (pchar 'X') <|> (pchar 'y') <|> (pchar 'Y') <|> (pchar 'z') <|> (pchar 'Z')) |>> Char.ToLower <!> "pdir"

let repeat = (pright (pstr "repeat ") (parse5 pname  
                                    (pright (pstr " starting at ") coords) 
                                    (pright (pstr " for ") pposnum)
                                    (pright (pstr " times, ") pposnum)
                                    (pright (pstr " apart in ") (pleft pdir (pstr " direction"))))
                                    |>> (fun (name,coords,times,sep,dir) -> Repeat(name,coords,times,sep,dir))) <!> "Repeat"

let command = ((pleft (build <|> define <|> repeat) (pmany0 pnl)) <|> endParser) <!> "Command"

let program = (pright pws0 (pmany1 command))

//loop through index of , find the next "load," get filename, load in file to string, replace "load filename" with string
let rec loadFilesInProgram (str: string) = 
    if (str.IndexOf "load ") = -1 then 
        str 
    else 
        try 
            let i = str.IndexOf "load"
            let filename = str.Substring(i+5,(str.IndexOf(".mpb") - (i + 5))) + ".mpb"
            
            //Trace.Listeners.Add(new TextWriterTraceListener(Console.Out)) |> ignore
            //Trace.WriteLine("Directory: " + Environment.CurrentDirectory)
            
            let bp = String.concat "\n" (File.ReadAllLines (filename)) //+ "\n"
            let loadFilename = "load " + filename
            loadFilesInProgram(str.Replace(loadFilename, bp))
        with 
        | :? System.IO.FileNotFoundException -> 
            let i = str.IndexOf "load"
            let filename = str.Substring(i+5,(str.IndexOf(".mpb") - (i + 5))) + ".mpb"
            raise (NoFileFoundException ("No file found to load:",filename))

let rec removeComments (str: string) = 
    if (str.IndexOf "//") = -1 then
        str
    else
        let startIndex = str.IndexOf "//"
        let endIndex = str.IndexOf("\n", startIndex)
        let length = endIndex - startIndex
        removeComments(str.Remove(startIndex,length))

let preprocessor (str: string) = 
    removeComments (loadFilesInProgram (removeComments str))

let prettyPrintCommand cmd = 
    match cmd with 
    | Build(name,coords) -> 
        let (Coords (x,y,z)) = coords
        "Build a " + name + " at (" + (string x) + "," + (string y) + "," + (string z) + ")"
    | Define(name,blueprint) -> 
        let (Blueprint sliceList) = blueprint
        let theFinalString = sliceList |> List.map (fun (Slice ls) -> ls) |> (List.map (fun ls -> List.map (fun (Line line) -> (stringify line)) ls)) |> (List.map (fun ls -> String.concat "\n\t" ls)) |> (List.fold (fun acc x -> acc + x + "\n\t-\n\t") "")
        "Define a " + name + " as \n{\n\t" + (theFinalString.Substring(0, theFinalString.Length-1)) + "}"
    | Repeat(name,coords,times,sep,dir) ->
         let (Coords (x,y,z)) = coords
         "Repeat " + (string times) + " " + name + " seperated by " + (string sep) + " in " + (string dir) + " direction, starting at (" + (string x) + "," + (string y) + "," + (string z) + ")"
    | End -> "End"

let prettyPrintProgram prgm =
     List.map prettyPrintCommand prgm |> String.concat "\n"

let grammar: Parser<Program> = pleft program peof

let parse input : Program option =
    let input' = prepare (preprocessor input)
    match grammar input' with
    | Success(res,_)     -> Some res
    | Failure(pos, rule) ->
        printfn "Invalid expression."
        let msg = sprintf "Cannot parse input at pos %d in rule '%s':" pos rule
        let diag = diagnosticMessage 20 pos input msg
        printf "%s" diag
        None