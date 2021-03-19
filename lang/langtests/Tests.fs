namespace langtests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open ProjectParser
open ProjectInterpreter
open System.Diagnostics

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.ParserASTCheck () =
        let input = "define struct as
{
    wWw
    WWW
    wWw
    -
    wWw
    WWW
    wWw
    -
    wWw
    WWW
    wWw
    -
}

build struct at (0,100,0)
end"
        let expected = 
                Some([Define
                   ("struct",
                    Blueprint
                      [Slice [Line ['w'; 'W'; 'w']; Line ['W'; 'W'; 'W']; Line ['w'; 'W'; 'w']];
                       Slice [Line ['w'; 'W'; 'w']; Line ['W'; 'W'; 'W']; Line ['w'; 'W'; 'w']];
                       Slice [Line ['w'; 'W'; 'w']; Line ['W'; 'W'; 'W']; Line ['w'; 'W'; 'w']]]);
                 Build ("struct",Coords (0,100,0)); End])

        let result = parse input
        
        Assert.AreEqual(expected, result)

    [<TestMethod>]
    member this.ParserASTCommentCheck () =
        let input = "define struct as
{
    wWw
    WWW
    wWw
    -
    wWw
    WWW
    wWw
    -
    wWw
    WWW
    wWw
    -
    // this is a comment that contains the word load
}
// this is also comment 
build struct at (0,100,0)
// comment
end"
        let expected = 
                Some([Define
                   ("struct",
                    Blueprint
                      [Slice [Line ['w'; 'W'; 'w']; Line ['W'; 'W'; 'W']; Line ['w'; 'W'; 'w']];
                       Slice [Line ['w'; 'W'; 'w']; Line ['W'; 'W'; 'W']; Line ['w'; 'W'; 'w']];
                       Slice [Line ['w'; 'W'; 'w']; Line ['W'; 'W'; 'W']; Line ['w'; 'W'; 'w']]]);
                 Build ("struct",Coords (0,100,0)); End])

        let result = parse input
        
        Assert.AreEqual(expected, result)

    [<TestMethod>]
    member this.InterpreterPlaceBlueprintCheck () =
        let input = Blueprint
                      [Slice [Line ['w'; 'W'; 'w']; Line ['W'; 'W'; 'W']; Line ['w'; 'W'; 'w']];
                       Slice [Line ['w'; 'W'; 'w']; Line ['W'; 'W'; 'W']; Line ['w'; 'W'; 'w']];
                       Slice [Line ['w'; 'W'; 'w']; Line ['W'; 'W'; 'W']; Line ['w'; 'W'; 'w']]]
                           
        let expected = 
            "\nmc.setBlock(0, 0, 0, 17)" +
            "\nmc.setBlock(1, 0, 0, 5)" +
            "\nmc.setBlock(2, 0, 0, 17)" +
            "\nmc.setBlock(0, 0, 1, 5)" +
            "\nmc.setBlock(1, 0, 1, 5)" +
            "\nmc.setBlock(2, 0, 1, 5)" +
            "\nmc.setBlock(0, 0, 2, 17)" +
            "\nmc.setBlock(1, 0, 2, 5)" +
            "\nmc.setBlock(2, 0, 2, 17)" +
            "\nmc.setBlock(0, 1, 0, 17)" +
            "\nmc.setBlock(1, 1, 0, 5)" +
            "\nmc.setBlock(2, 1, 0, 17)" +
            "\nmc.setBlock(0, 1, 1, 5)" +
            "\nmc.setBlock(1, 1, 1, 5)" +
            "\nmc.setBlock(2, 1, 1, 5)" +
            "\nmc.setBlock(0, 1, 2, 17)" +
            "\nmc.setBlock(1, 1, 2, 5)" +
            "\nmc.setBlock(2, 1, 2, 17)" +
            "\nmc.setBlock(0, 2, 0, 17)" +
            "\nmc.setBlock(1, 2, 0, 5)" +
            "\nmc.setBlock(2, 2, 0, 17)" +
            "\nmc.setBlock(0, 2, 1, 5)" +
            "\nmc.setBlock(1, 2, 1, 5)" +
            "\nmc.setBlock(2, 2, 1, 5)" +
            "\nmc.setBlock(0, 2, 2, 17)" +
            "\nmc.setBlock(1, 2, 2, 5)" +
            "\nmc.setBlock(2, 2, 2, 17)\n"

        let result = placeBlueprint (input) (Coords(0,0,0)) ("")

        Assert.AreEqual(result, expected)
 
    [<TestMethod>]
    member this.InterpreterRepeatCheck () =
        let (start: Environment) = (Map.empty,"")
        let input = Blueprint
                      [Slice [Line ['w'; 'w']];
                       Slice [Line ['w'; 'w']]]
        let env = fst start   
        let e = (env.Add("test",input),"") 
                           
        let expected = 
            "\nmc.setBlock(0, 0, 0, 17)" +
            "\nmc.setBlock(1, 0, 0, 17)" +
            "\nmc.setBlock(0, 1, 0, 17)" +
            "\nmc.setBlock(1, 1, 0, 17)" +
            "\n\n\nmc.setBlock(0, 4, 0, 17)" +
            "\nmc.setBlock(1, 4, 0, 17)" +
            "\nmc.setBlock(0, 5, 0, 17)" +
            "\nmc.setBlock(1, 5, 0, 17)" +
            "\n\n\nmc.setBlock(0, 8, 0, 17)" +
            "\nmc.setBlock(1, 8, 0, 17)" +
            "\nmc.setBlock(0, 9, 0, 17)" +
            "\nmc.setBlock(1, 9, 0, 17)\n"
           
        let result = evalRepeat "test" (Coords(0,0,0)) 3 2 'y' e
        let str = snd result

        Assert.AreEqual(str, expected)