#r "../bin/AbSyn.dll"
#r "../lib/FSharp.PowerPack.dll"
#r "../bin/Parser.dll"
#r "../bin/Lexer.dll"
#r "../bin/Types.dll"
#r "../bin/SymTab.dll"
#r "../bin/Compose.dll"
#r "../bin/Transformations.dll"
#r "../bin/Compose.dll"
#r "../bin/MIDI.dll"

open System.Text
open System.IO

open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing

let parse (s : string) : AbSyn.UnchekedDeclaration list =
  Parser.Program Lexer.Token
  <| LexBuffer<_>.FromBytes (Encoding.UTF8.GetBytes s)
let compose          = Compose.compose "main"
let tomidi           = MIDI.getPymidi

let help () =
  printfn ""
  printfn @"     *********************************************"
  printfn @"     *  Welcome to the F♭ MIDI compiler. {^o^}   *"
  printfn @"     *********************************************"
  printfn @"     *┈┈┈┈┏━┓┈┈┈┈┈┏╯┈┈┈┈┏╯┈┈┏━┓┈┈┈┈┈┏╯┈┈┈┈┏╯┈┈┈┈┈*"
  printfn @"     *┈┈┈┈┣━┫┈┈┈┈┈┣╯┈┈┈┈┣╯┈┈┣━┫┈┈┈┈┈┣╯┈┈┈┈┣╯┈┈┈┈┈*"
  printfn @"     *┈┈┈╭┫╭┫┈┈┃┈╭┫┈┈┃┈╭┫┈┈╭┫╭┫┈┈┃┈╭┫┈┈┃┈╭┫┈┈┈┈┈┈*"
  printfn @"     *┈┈┈╰╯╰╯┈╭┫┈╰╯┈╭┫┈╰╯┈┈╰╯╰╯┈╭┫┈╰╯┈╭┫┈╰╯┈┈┈┈┈┈*"
  printfn @"     *┈┈┈┈┈┈┈┈╰╯┈┈┈┈╰╯┈┈┈┈┈┈┈┈┈┈╰╯┈┈┈┈╰╯┈┈┈┈┈┈┈┈┈*"
  printfn @"     *********************************************"
  printfn @"     * Inorder to properly produce a pymidi util *"
  printfn @"     * file, you must specify 3 flags            *"
  printfn @"     *                                           *"
  printfn @"     * -o : 'a name for the python midifile'     *"
  printfn @"     *                                           *"
  printfn @"     * -t : 'a tempo in beats per minute'        *"
  printfn @"     *                                           *"
  printfn @"     * -T : [optinal] 'a project title'          *"
  printfn @"     *                                           *"
  printfn @"     *********************************************"
  printfn @"     * The basic usage is:                       *"
  printfn @"     * ./fflat -t time -o outfile fflatfile      *"
  printfn @"     *********************************************"
  printfn ""

let pos2str = function
  | (-1, _) -> sprintf "\nPositions have not yet been implemented for this error."
  | (-2, d) -> sprintf "in measure %d" d
  | ( l, c) -> sprintf "at line %d in column %d" l c

let msgAndPos s p = printfn "%s%s" s (pos2str p)

let parseProgram (filename : string) : AbSyn.UnchekedDeclaration list =
  let programtext =
    match File.ReadAllText filename with
      | ""  -> failwith "Empty file !"
      | txt -> txt
  let program =
    try
      parse programtext
    with
      | Lexer.LexicalError (msg, pos)      -> msgAndPos msg pos ; exit 1; []
      | ex when ex.Message = "parse error" ->
        printfn "Parser error : TODO -> print possition of perr"; exit 1; []
      | ex -> printfn "%A" ex.Message ; exit 1 ; []
  program

let run tempo title program =
  (compose >> tomidi tempo title) (parseProgram program)

[<EntryPoint>]
let main (args: string[]) : int =
  try
    match args with
      | [| "-t" ; tempo ; "-o" ; outfile;        file |] ->
        let output = new StreamWriter (outfile)
        output.Write (run (System.Int32.Parse tempo) "song" file)
        output.Flush ()
        output.Close ()
      | [| "-t" ; tempo ; "-o" ; outfile; "-T" ; title; file |] ->
        let output = new StreamWriter (outfile)
        output.Write (run (System.Int32.Parse tempo) title file)
        output.Flush ()
        output.Close ()
      | _ -> help()
  with
    // TODO : this should be massively extended if anybody is going
    //        to be using the language for realz.             {~_^}
    | :? FileNotFoundException as e -> printfn "%A" e.Message
    | Transformations.VerificialError (msg, pos) -> msgAndPos msg pos
    | Transformations.TransformationError msg    -> msgAndPos msg (-1, 0)
    | Failure message                            -> printfn "%s" message
    | Compose.CompositorialError msg             -> msgAndPos msg (-1, 0)
    | ex -> printfn "Unhanled exception %A" ex
  0
