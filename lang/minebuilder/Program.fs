open System
open System.IO
open ProjectParser
open ProjectInterpreter

[<EntryPoint>]
let main argv =
    try 
        let (start_enviro: Environment) = (Map.empty,"import sys\ntry:\n\tfrom mcpi.minecraft import Minecraft\n\tmc = Minecraft.create()\nexcept:\n\tprint(\"ERROR\")\n\tsys.exit(1)")
        let program = (File.ReadAllText argv.[0])
        //let printer = (int argv.[1])
        match (parse program) with
        | Some ast ->  
                let ending_enviro = (evalProgram ast start_enviro)
                if (snd start_enviro) = (snd ending_enviro) then
                    printf "Note: Nothing has been built, as no build or repeat statements found in source.\nPlease add build/repeat statements and try again!"
                else 
                    printf "Program successfully executed. Please check Minecraft to see results!\nAdditionally, check mine.py for the Python interpretation of your source code."
        | None -> printfn "Failed to parse expression. Please check the program and try again!"
    with 
    | IncorrectCharacterException(_, char) -> 
        printfn "ERROR: Incorrect character in one or more blueprints: %c%s" char "\nPlease check the program and try again!"
        exit 1 
    | IncorrectNameException(_,name) -> 
        printfn "ERROR: Forbidden name in definition: %s%s" name "\nPlease check the program and try again!"
        exit 1 
    | BlueprintNotDefinedException(_,name) -> 
        printfn "ERROR: Blueprint not defined: %s%s" name "\nPlease check the program and try again!"
        exit 1 
    | UndefinedDirectionException(_, char) ->
        printfn "ERROR: Undefined direction in repeat statement: %c%s" char "\nPlease check the program and try again!"
        exit 1
    | NoFileFoundException(_, name) ->
        printfn "ERROR: File not found in load statement: %s%s" name "\nPlease check the program and try again!"
        exit 1
    | OperatingSystemException(msg) ->
        printfn "ERROR: Problem running program on this OS: %s%s" msg "\nPlease check the program and try again!"
        exit 1
    | PythonScriptException(msg) ->
        printfn "ERROR: Problem running Python Script: %s%s" msg "\nPlease check the program and try again!"
        exit 1
    | :? FileNotFoundException as ex ->
        printfn "ERROR: Could not find program file: %s%s" (Path.GetFileName(ex.FileName)) "\nPlease check the program and try again!"
        exit 1
    | err ->
        printfn "ERROR: %A\n%s" err "\nUsage: dotnet run <program.mc>\n<program.mc> must be a valid filename"
        exit 1
    0
