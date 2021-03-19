module ProjectInterpreter
open System.IO
open System
open ProjectParser
open System.Diagnostics;

type Environment = Map<string,Blueprint> * string

exception IncorrectCharacterException of string * char 
exception IncorrectNameException of string * string
exception BlueprintNotDefinedException of string * string
exception UndefinedDirectionException of string * char 
exception OperatingSystemException of string
exception PythonScriptException of string

let placeBlock (block : char)(coords : Coords) = 
    let (Coords (x,y,z)) = coords
    let s = "mc.setBlock("+ (string x) + ", " + (string y) + ", " + (string z) + ", "
    match block with
    | '.' -> ""
    | 'S' -> s + "1)\n"
    | 'R' -> s + "2)\n"
    | 'D' -> s + "3)\n"
    | 'C' -> s + "4)\n"
    | 'W' -> s + "5)\n"
    | 's' -> s + "12)\n"
    | 'r' -> s + "13)\n"
    | '1' -> s + "14)\n"
    | '2' -> s + "15)\n"
    | '3' -> s + "16)\n"
    | 'w' -> s + "17)\n"
    | 'l' -> s + "18)\n"
    | '~' -> s + "20)\n"
    | '4' -> s + "21)\n"
    | 'L' -> s + "22)\n"
    | 'N' -> s + "24)\n"
    | 'P' -> s + "35)\n"
    | 'G' -> s + "41)\n"
    | 'I' -> s + "42)\n"
    | '?' -> s + "43)\n"
    | '@' -> s + "44)\n"
    | 'B' -> s + "45)\n"
    | 't' -> s + "46)\n"
    | 'b' -> s + "47)\n"    
    | 'M' -> s + "48)\n"
    | 'O' -> s + "49)\n"
    | '+' -> s + "54)\n"
    | '5' -> s + "56)\n"
    | 'd' -> s + "57)\n"
    | '$' -> s + "58)\n"
    | 'F' -> s + "62)\n"
    | '6' -> s + "73)\n"
    | 'i' -> s + "79)\n"
    | '^' -> s + "80)\n"
    | 'c' -> s + "82)\n"
    | 'H' -> s + "85)\n"
    | '&' -> s + "89)\n"
    | '*' -> s + "98)\n"
    | 'p' -> s + "102)\n"
    | 'm' -> s + "103)\n" 
    | 'o' -> s + "246)\n"
    | _ -> raise (IncorrectCharacterException ("Incorrect character in encoding:",block))

let placeLine (line: Line)(coords : Coords) = 
    let (Coords (x,y,z)) = coords
    let (Line lst) = line
    List.mapi (fun i block -> (i,block)) lst
        |> List.fold (fun acc (i,block) -> acc + (placeBlock block (Coords(x+i,y,z)))) "" 

let placeSlice (slice : Slice)(coords : Coords) =
    let (Coords (x,y,z)) = coords
    let (Slice lst) = slice
    List.mapi (fun i line -> (i,line)) lst
        |> List.fold (fun acc (i,line) -> acc + (placeLine line (Coords(x,y,z+i)))) "" 

let placeBlueprint (bp : Blueprint)(coords : Coords)(start: string) = 
    let (Coords (x,y,z)) = coords
    let (Blueprint lst) = bp
    List.mapi (fun i slice -> (i,slice)) lst
        |> List.fold (fun acc (i,slice) -> acc + (placeSlice slice (Coords(x,y+i,z)))) (start + "\n") 
        
let writePython instr = 
    File.WriteAllText("mine.py",instr)

let doUnixPython instr =
    try 
        let startInfo = ProcessStartInfo("python3","mine.py")

        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true
        let p = Process.Start(startInfo)

        let out = p.StandardOutput.ReadToEnd()
        if out = "ERROR\n" then
            raise (PythonScriptException "")
        p.WaitForExit()
    with 
    | _ -> 
        raise (PythonScriptException ("Improper setup of Minecraft environment! \nLook at mine.py for Python script of interpreted output."))

let doWinPython instr =
    try 
        let path = "mine.py"
        let strCmdText = "/C python " + path
        
        let startInfo = ProcessStartInfo("CMD.exe", strCmdText)

        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true
        let p = Process.Start(startInfo)

        let out = p.StandardOutput.ReadToEnd()
        printfn "%s" out
        if out = "ERROR
"       
        then
            raise (PythonScriptException "")
        p.WaitForExit()
    with 
    | _ -> 
        raise (PythonScriptException ("Improper setup of Minecraft environment! \nLook at mine.py for Python script of interpreted output."))

let finish prog = 
    writePython prog 
    let platform = Environment.OSVersion.Platform
    match platform with
    | PlatformID.Unix -> 
        doUnixPython prog
    | PlatformID.Win32NT ->
        doWinPython prog  
    | _ -> 
        raise (OperatingSystemException ("Unsupported Operating System!"))

let widestPoint (bp : Blueprint) dir = 
    let (Blueprint lst) = bp
    match dir with
    | 'x' -> 
        let lineLst = lst |> List.map (fun slice -> match slice with Slice lnLst -> lnLst) 
        lineLst |> List.concat |> List.fold (fun acc line ->
                                    let (Line lnLst) = line
                                    (max acc (lnLst.Length))) 0 
    | 'y' -> 
        lst.Length
    | 'z' -> 
        lst |> List.fold (fun acc slice ->
                          let (Slice lnLst) = slice
                          (max acc (lnLst.Length))) 0 
    | _ -> raise (UndefinedDirectionException ("Undefined direction in repeat statement:",dir))

let evalRepeat name coords times sep dir (enviro : Environment) : Environment = 
    try 
        let (Coords (x,y,z)) = coords
        let e = fst enviro
        let prog = snd enviro
        let bp = e.[name]
        let w = widestPoint bp dir 
        let sepdList = match dir with
                       | 'x' -> 
                             List.init times (fun i -> (bp,(Coords(x+(i*(w+sep)),y,z))))
                       | 'y' ->
                             List.init times (fun i -> (bp,(Coords(x,y+(i*(w+sep)),z))))
                       | 'z' ->
                             List.init times (fun i -> (bp,(Coords(x,y,z+(i*(w+sep))))))
                       | _ -> raise (UndefinedDirectionException ("Undefined direction in repeat statement:",dir))
        let repeatedCodeLst = sepdList |> List.map (fun (bloop,coords) -> (placeBlueprint bloop coords ""))
        let prog2 = prog + (String.concat "\n" repeatedCodeLst)
        (e,prog2)
    with 
    | :? System.Collections.Generic.KeyNotFoundException ->
         raise (BlueprintNotDefinedException ("Blueprint not defined:",name))
     
let evalBuild name coords (enviro : Environment) : Environment = 
    try 
        let e = fst enviro
        let prog = snd enviro
        let bp = e.[name]
        (e,placeBlueprint bp coords prog) 
    with 
    | :? System.Collections.Generic.KeyNotFoundException ->
         raise (BlueprintNotDefinedException ("Blueprint not defined:",name))

let evalCommand (c: Command)(e: Environment) : Environment = 
    match c with 
    | Define(name,bp) -> 
        if name.StartsWith("ERROR") then
              raise (IncorrectNameException ("Forbidden name for definition!",name.Substring(5)))
        else 
            let env = fst e
            let prog = snd e
            (env.Add(name,bp),prog) 
    | Build(name,coords) -> evalBuild name coords e
    | Repeat(name,coords,times,sep,dir) -> evalRepeat name coords times sep dir e 
    | End ->
        let prog = (snd e)
        finish prog 
        e

let rec evalProgram (p: Program)(e: Environment) : Environment =
    match p with
    | c::cs ->
        let e' = evalCommand c e
        evalProgram cs e'
    | [] -> e

