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

open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing
open System.Text
open System.IO

open AbSyn
open Compose
open Types

let parse (s : string) =
  Parser.Program Lexer.Token <| LexBuffer<_>.FromBytes(Encoding.UTF8.GetBytes s)

let print s = printfn "%A" s
let file   = File.ReadAllText "../tests/ff_files/test2.fflat"
let decls  = parse file

// let compose (name : string) (decls : UnchekedDeclaration list) =
let (compositions, pieces, phrases) = splitDeclarations decls
let (rt, mt)                        = pieces
let (rpt, mpt, it, nt)              = phrases
let main =
  match SymTab.Lookup "main" compositions with
    | None          -> failwith "could not find 'main'"
    | Some (e, _)   -> e

// in eval compositions declarations pieces phrases exp)
// Case exp of : Assignment (var x, (Simple y)) ->
let (instrument, notes) = initMelody decls phrases (Var "x")
let (sign, qbs)         = getBeats pieces phrases "y"
let stream              = streamOfBeats qbs
let length              = List.reduce (+) stream
let result = (instrument, sign, length, mkChanel (notes, stream))

// let cast (sign, qbs) sigh' = List.map (( * )
// (List.map (Q.Frac >> ( * ) (x / y)) [(1 , 4); (1 , 4); (1, 4); (1, 4)])

let a = Sig (6, 4)
let b = Q.Frac (6, 4)
print (a, b)


let rec phrasing s = s

// print (qbs, sign, main)

open Types
let x = Q.Frac (4, 4)
let y = Q.Frac (3, 4)
