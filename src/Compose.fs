
module Compose

open Types
open AbSyn

// We split Unchecked Declarations into compositional building blocks
// using the transformations library
let splitDeclarations = Transformations.collectCompositions

// These errors will be improved later
exception CompositorialError of string
let err s = raise (CompositorialError s)
let nerr name nt etype =
  failwith (Transformations.findnameerror name nt etype)

// For time casting {^o^}
let sig2frac (Sig (n, d)) = Q.Frac (n, d)

// Scales a beat with timesignature n to fit a signature m
let rec scaleBar n m = function
  | H f :: bs -> H ((n / m) * f) :: scaleBar n m bs
  | R f :: bs -> R ((n / m) * f) :: scaleBar n m bs
  | _         -> []

let rec scaleBars n m = function
  | QB qb :: qbs -> QB (scaleBar n m qb) :: scaleBars n m qbs
  | _         -> []

// For the purpose of diatonic scale casting
let rec findmelody name nt = function
  | ComposedMelody (name', i, k  , ns, p) :: _ when name = name' ->
    (name, i, k  , ns, p)
  | decl :: decls -> findmelody name nt decls
  | _             -> nerr name nt "melody"

// Finds a melodic declaration, and replaces the keysignature
let replaceKey name key nt decls =
  let (_, i, _, ns, p) = findmelody name nt decls in
  ComposedMelody (name, i, key, ns, p)

// Recomputes a smaller mapping of melodies an rythms
let mkMap = Transformations.verifyPieces

// In most cases, we know it is there because i just generated it!
let unsafe_lookup name table =
  match SymTab.Lookup name table with
    | None       -> err (sprintf "unsafe lookup error ! %s" name)
    | Some thing -> thing

// Replace the key signature of a melody, and recompute the absolute notes
let transpose key name decls ((_, _, _, nt) as phrases) =
  let mt = snd (mkMap [replaceKey name key nt decls] phrases) in
  unsafe_lookup name mt

// From now on, a melody is just an infinite sequence of absolute notes.
// Played by some MIDI instrument
let initMelody decls phrases =
  let (_, _, _, nt) = phrases
  function
   | Var name ->
     let mt = snd (mkMap [ComposedMelody (findmelody name nt decls)] phrases) in
     let (instrument, _ , notes, _) = unsafe_lookup name mt
     (instrument, Seq.initInfinite (fun n -> notes.[n % (List.length notes)]))
   | MCast (name, key) ->
     let (instrument, _ , notes, _) = transpose key name decls phrases
     (instrument, Seq.initInfinite (fun n -> notes.[n % (List.length notes)]))
   | m        -> (err (sprintf "Invalid melody %A" m))

// Looks up a (beat list) and time signature
// This is going to be more complicated with tied beats so i passed
// some extra parameters which we are not using yet
let getBeats (rt, _) (_, _, _, nt) name =
  let (sign, beats) =
    match SymTab.Lookup name rt with
      | None                  -> nerr name nt "rythmic phrases"
      | Some (sign, beats, _) -> (sign, beats)
  (sign, beats)

// A channel is just an assignment of absolute notes to a list of beats
let rec mkChanel = function
  | tones, (H q :: hits) ->
    match Seq.head tones with
      | S _    -> err "scale steps in compositions not implemented yet"
      | N tone -> (tone, H q) :: mkChanel (Seq.tail tones, hits)
  | tones, (R q :: hits) ->
    ((C, 0, 0, 0), R q) :: mkChanel (tones, hits)
  | (_ : seq<MusicNote>) , ([] : Beat list) -> [] : Chanel

// TODO : handle tied beats
let unpackQb = function
  | QB beats -> beats
  | T _      -> err "Unimplemented feature 'tied beats'"
let streamOfBeats = List.collect id << (List.map unpackQb)

// Complicated phrasing is a new way of combining ryhtms {~_^}
// for the time cast
// printfn "%A" (List.map (Q.Frac >> ( * ) (x / y)) [(1, 2); (1, 2)])
// type Phrasing =
//   | Simple       of string
//   | Repetition   of Phrasing * int
//   | Union        of Phrasing * Phrasing
//   | Intersection of Phrasing * Phrasing
//   | Subtraction  of Phrasing * Phrasing
//   | Projection   of Phrasing * string
//   | RCast        of Phrasing * Signature

// Evaluates a compositional expression
let rec eval cs ds pis phs = function
  | Assignment (mel, (Simple ryth)) ->
      let (instrument, notes) = initMelody ds phs mel
      let (sign, qbs)         = getBeats pis phs ryth
      let stream  = streamOfBeats qbs
      let length  = List.reduce (+) stream
      [[instrument, sign, length, mkChanel (notes, stream)]]
  | Assignment (mel, ryth) -> err "Complicated Rythmic Phrasing is not implemented"
  | Simultanious ((Assignment (_, _) as comp1), comp2) ->
    let channel1 = (eval cs ds pis phs comp1).[0].[0]
    let channel2 = (eval cs ds pis phs comp2).[0]
    [channel1 :: channel2]
  | Simultanious (_, _)   -> err "+ is only supported for asignment statements !"
  | Before (comp1, comp2) -> eval cs ds pis phs comp1 @ eval cs ds pis phs comp2
  | After  (comp2, comp1) -> eval cs ds pis phs comp1 @ eval cs ds pis phs comp2
  | Var name              ->
    match SymTab.Lookup name cs with
      | Some (e, _) -> eval cs ds pis phs e
      | None        ->
        let (_, _, _, nt) = phs in nerr name nt "compositional building block"
  | MCast _ -> err "diatonic casting is not well defined at this point"

// Compose breaks down the composition AST into channels for the midi generator
let compose (name : string) (decls : UnchekedDeclaration list) =
  let (compositions, pieces, phrases) = splitDeclarations decls
  let (rt, mt)                        = pieces
  let (rpt, mpt, it, nt)              = phrases
  match SymTab.Lookup name compositions with
    | None          -> nerr name nt "compositional building block"
    | Some (e, _)   -> (eval compositions decls pieces phrases e, it, nt)
