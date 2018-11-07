
module Transformations (* A module for verifying music theoretic properties *)

open Types
open AbSyn

// In this file we will use symbol tables for keeping track of everything {^.^}
type rythm_table   = SymTab.Table<Signature * QBeats list * Position>
type melody_table  = SymTab.Table<MidiInstrument * Key * (MusicNote list) * Position>
type rphrase_table = SymTab.Table<RythmNote list * Position>
type mphrase_table = SymTab.Table<Note      list * Position>
type midi_table    = SymTab.Table<int * int      * Position>
type composition   = SymTab.Table<Composition    * Position>
type name_table    = SymTab.Table<string         * Position>

exception VerificialError     of string * (int * int)
exception TransformationError of string

// functions for raising errors in a less verbose way
let verr s p  = raise (VerificialError (s, p))
let terr s    = raise (TransformationError s)

// Check names inside declarations
let findnameerror name table etype =
  match SymTab.Lookup name table with
    | None           -> sprintf "Unknown variable name '%s' " name
    | Some (id, pos) ->
      sprintf "Name '%s' was declared to be a %s but was used as a %s " name id etype

// Check names at declaration level
let checkName id name pos nametab =
  match SymTab.Lookup name nametab with
    | None   -> (SymTab.Bind name (id, pos) nametab)
    | Some i ->
      verr (sprintf "Name '%s' is used for multiple variables " name) pos

// Builds a directed Graph ..
// from a symbol table and a function that returns the edges
let buildGraph f symtab =
  let referenceList = List.map (fun (n, (r, _)) -> (n, V (f r)))
  in (Graph.OfList << referenceList << SymTab.ToList) symtab

// Try to build a sorted list of nodes and fail if this is not possible
let checkDAG graph etype nametable =
  try
    // We don't care about the result in this case, (only the potential exn)
    let _ = Graph.DebthFirstSearch graph in ()
  with
    | UnknownVariableName name  ->
      verr (findnameerror name nametable etype) (-1,0)
    | CyclicReference     _     ->
      let msg = sprintf "There was a cyclic reference path in your %s declarations"
      verr (msg etype) (-1,0)

// Returns 4 tables.
// rpt : Rythmic Phrase Table
// mpt : Melodic Phrase Table
// it  : (midi) Instrument Table
// nt  : Name Table (a look up for all names and their category)
// The latter is usefull for returning reasonable error messages.
let rec getPhrases rpt mpt it nt = function
 | RythmicPhrase (name, notes, pos) :: decls ->
   let rpt' = (SymTab.Bind name (notes, pos) rpt) in
   getPhrases rpt' mpt it (checkName "rythmic phrase" name pos nt) decls
 | MelodicPhrase (name, notes, pos) :: decls ->
   let mpt' = (SymTab.Bind name (notes, pos) mpt) in
   getPhrases rpt mpt' it (checkName "melodic phrase" name pos nt) decls
 | InstrumentDeclaration (name, sfid, volume, pos) :: decls ->
   let it'  = (SymTab.Bind name (sfid, volume, pos) it) in
   getPhrases rpt mpt it' (checkName "midi instrument" name pos nt) decls
 | Beat (name, _, _, pos) :: decls ->
   getPhrases rpt mpt it (checkName "rythm" name pos nt) decls
 | Repeat (name, _, _, _, pos) :: decls ->
   getPhrases rpt mpt it (checkName "rythm" name pos nt) decls
 | ComposedMelody (name, _, _, _, pos) :: decls ->
   getPhrases rpt mpt it (checkName "melody" name pos nt) decls
 | CompositionDeclaration (name, _, pos) :: decls ->
   getPhrases rpt mpt it (checkName "composition" name pos nt) decls
 | []             ->
   let rg, mg = buildGraph findRPhrases rpt, buildGraph findMPhrases mpt
   checkDAG rg "rythmic phrase" nt
   checkDAG mg "melodic phrase" nt
   (rpt, mpt, it, nt)

// Find names of rythmic phrases
and findRPhrases = function
  | PhraseID n :: rns -> n :: findRPhrases rns
  | _          :: rns ->      findRPhrases rns
  | _                 -> []

// Find names of melodic phrases
and findMPhrases = function
  | NotePhrase n :: rns -> n :: findMPhrases rns
  | _            :: rns ->      findMPhrases rns
  | _                   -> []

(****************************************)
(**************** RYTHMS ****************)
(****************************************)

// Builds a complete list of measures in a rythmic phrase
let rec buildMeasureList c rythm rtab nametable = function
  | Single m :: ms ->
    buildMeasure 0 rythm rtab nametable m ::
    buildMeasureList (c + 1) rythm rtab nametable ms
  | []             -> []
  | _              -> terr "Tied measures is not yet implemented"

// Builds a single rythmic measure
and buildMeasure c rythm rtab nametable = function
  | PhraseID n :: ns ->
    match SymTab.Lookup n rtab with
      | None              ->
        verr (findnameerror n nametable "basic rythm block") (-1,0)
      | Some (notes, _) ->
        buildMeasure c rythm rtab nametable notes @
        buildMeasure c rythm rtab nametable ns
  |     Rest  n :: ns  ->
    R (noteValue c rythm n) :: buildMeasure c rythm rtab nametable ns

  |     Beam ns :: ns' -> buildMeasure c rythm rtab nametable ns @
                          buildMeasure c rythm rtab nametable ns'
  |           n :: ns  ->
    H (noteValue c rythm n) :: buildMeasure c rythm rtab nametable ns
  | []                 -> []

// Computes the fraction value of a single beat
and noteValue c rythm = function
  | RNote    n  -> Q.Frac (1, n)
  | Flagged  n  -> Q.Frac (1, 2) * noteValue c rythm n
  | Dotted   n  -> Q.Frac (3, 2) * noteValue c rythm n
  | Triplet  n  -> Q.Frac (2, 3) * noteValue c rythm n
  | Tie      ns -> List.reduce (+) (List.map (noteValue c rythm) ns)
  | _ ->
    terr (sprintf ("this is not the case in noteValue in %s ") rythm)

(****************************************)
(**************** MELODIES **************)
(****************************************)

// Music scale arithmetic based on 3 views of the chromatic scale {^_^}
let c1 = [NoteC 0; Sharp (NoteC 0); NoteD 0; Sharp (NoteD 0);
          NoteE 0; NoteF 0; Sharp (NoteF 0); NoteG 0; Sharp (NoteG 0);
          NoteA 0; Sharp (NoteA 0); NoteB 0]

let c2 = [NoteC 0; Flat (NoteD 0); NoteD 0; Flat (NoteE 0);
          NoteE 0; NoteF 0; Flat (NoteG 0); NoteG 0; Flat (NoteA 0);
          NoteA 0; Flat (NoteB 0); NoteB 0]

let c3 = [Sharp (NoteB (-1)); Flat (NoteD 0); NoteD 0; Flat (NoteE 0);
          Flat (NoteF 0); Sharp (NoteE 0); Flat (NoteG 0); NoteG 0;
          Flat (NoteA 0); NoteA 0; Flat (NoteB 0); Flat (NoteC 1)]

// Shift list 'l' (of length 'n') by m
let shift (n : int) (m : int) (l : 'a list) : 'a list =
  (Seq.toList (Seq.take (m + n) (Seq.initInfinite (fun i -> l.[i % n])))).[m..]

// The steps of the ionian scale (1 for whole step, 0 for half step)
let ionian     = [1; 1; 0; 1; 1; 1; 0]

// Not really nessesary but speeds some things up
let unique = Set.ofList >> Set.toList

// The diatonic scales are just inversions of the ionian scale
let diatonic mode = shift 7 mode ionian

// Translates a melodic mode into a number of shifts and a scale key
let modePair = function
  | Ionian     key -> 0, key
  | Dorian     key -> 1, key
  | Phrygian   key -> 2, key
  | Lydian     key -> 3, key
  | Mixolydian key -> 4, key
  | Aeolian    key -> 5, key
  | Locrian    key -> 6, key

// Moves a music note to the n'th octave
let rec get n = function
  | NoteC _         -> NoteC n
  | NoteD _         -> NoteD n
  | NoteE _         -> NoteE n
  | NoteF _         -> NoteF n
  | NoteG _         -> NoteG n
  | NoteA _         -> NoteA n
  | NoteB _         -> NoteB n
  | Step (m, _) -> Step(m, n)
  | Sharp note  -> Sharp (get n note)
  | Flat  note  -> Flat  (get n note)
  |       note  -> verr (sprintf "Invalid key signature %A" note) (-1, 0)

// Moves a music note to the (f n)'th octave
let rec getWith f = function
  | NoteC n         -> NoteC (f n)
  | NoteD n         -> NoteD (f n)
  | NoteE n         -> NoteE (f n)
  | NoteF n         -> NoteF (f n)
  | NoteG n         -> NoteG (f n)
  | NoteA n         -> NoteA (f n)
  | NoteB n         -> NoteB (f n)
  | Step (m, n) -> Step(m, f n)
  | Sharp note  -> Sharp (getWith f note)
  | Flat  note  -> Flat  (getWith f note)
  |       note  -> verr (sprintf "Invalid key signature %A" note) (-1, 0)

// Find a musical note's index into a chromatic scale
let getIndex key =
  let search = List.zip [0..11] >> List.tryFind (fun (step, n') -> n' = (get 0 key)) in
  match  search c1, search c2, search c3 with
    | Some (step, _), _, _ -> step
    | _, Some (step, _), _ -> step
    | _, _, Some (step, _) -> step
    | _                    -> verr (sprintf "invalid key '%A'" key) (-1, 0)

// Splits a list at the n'th element
let rec split n = function
  | []                      -> ([], [])
  | x :: _ as xs when n = 0 -> ([], xs)
  | x :: xs                 -> let (a, b) = split (n - 1) xs in (x :: a, b)

// referes one of the 3 views of the chromatic scale
let chromatic' = function
  | 1 -> c1
  | 2 -> c2
  | 3 -> c3
  | _ -> terr "This never happens ! - chromatic'"

// Gives a relative scale step view of the chromatic scale
// (To keep intervals persistant)
let chromatic key view =
  let (a, b) = split (getIndex key) (chromatic' view) in
  (List.map (getWith ((+) 1)) a) @ b


// Computes the chromatic steps of a diatonic scale
// (fill in the missing 4 notes)
let rec diatonicSteps f d c = function
  | 0 :: ds ->
    Step (c, 0)   :: diatonicSteps f d (c + 1) ds
  | 1 :: ds ->
    (Step (c, 0)) :: d (Step (f c, 0)) :: diatonicSteps f d (c + 1) ds
  | [] -> []
  | _  -> failwith "Not a Valid Diatonic Mode"

// Simplifies a musical note after the above transformations
// TODO : add more advanced rules {^.^}
let rec simplify = function
  | Sharp (Flat  note) -> simplify note
  | Flat  (Sharp note) -> simplify note
  | Sharp (NoteB n)    -> NoteC (n + 1)
  | Flat  (NoteC n)    -> NoteB (n - 1)
  | Sharp note         -> Sharp (simplify note)
  | Flat  note         -> Flat  (simplify note)
  | note               -> note

// Returns mappings (scalestep -> absolute node), (scalestep <- absolute node)
let scaleSteps scale =
  let (mode, key) = modePair scale
  let scale steps = shift 12 (getIndex key) >> List.zip steps
  let notes t =
    scale (diatonicSteps id      Sharp 1 (diatonic mode)) t @
    scale (diatonicSteps ((+) 1) Flat  1 (diatonic mode)) t
  let find i     = notes (chromatic key i)
  let chromatics = unique (find 1 @ find 2 @ find 3)
  let apply f (n, m) = (f n, f m)
  let doubles = List.map (apply Sharp) chromatics
  let doubleb = List.map (apply Flat ) chromatics
  let possibleNotes =
    unique (List.map (apply simplify) (chromatics @ doubles @ doubleb))
  (Map.ofList possibleNotes,
   Map.ofList ((List.map (fun (n, m) -> (m, n))) possibleNotes))

// Collects basic rythm blocks and melodic pieces
// By building measures and applying diatonic scale inversions
let rec getPieces rtab mtab tabs = function
 | Beat   (name, sign,    ms, pos) :: decls       ->
   let (rpt, _, _, nt) = tabs
   let beat       = joinRests (buildMeasureList 1 name rpt nt ms)
   let _          = checkSum name 1 sign beat
   getPieces (SymTab.Bind name (sign, makeQBeats beat,  pos) rtab) mtab tabs decls
 | Repeat (name, sign, m, ms, pos) :: decls       ->
   let (rpt, _, _, nt) = tabs
   let beat       = joinRests (buildMeasureList 1 name rpt nt ms)
   let _          = checkSum name 1 sign beat
   let beats      = makeQBeats (List.collect id (List.replicate m beat))
   getPieces (SymTab.Bind name (sign, beats, pos) rtab) mtab tabs decls
 | ComposedMelody (name, i, key, ns, pos) :: decls  ->
   let ( _, mpt, it, nt)  = tabs
   let stepNote, noteStep = scaleSteps key
   let octave             = getOctave (key2Note key)
   let notes              = translateNotes octave stepNote mpt noteStep ns
   getPieces rtab (SymTab.Bind name (i, key, notes, pos) mtab) tabs decls
 |  _ :: decls    -> getPieces rtab mtab tabs decls
 | []             -> rtab, mtab

// Translates scale steps into an absolute notes
and translateNotes octave key mpt inverse_key = function
  | (NotePhrase _ as phrase) :: notes ->
    translatePhrase octave key mpt [phrase] @
    translateNotes octave key mpt inverse_key notes
  | (Step _ as step) :: notes ->
    translateNote octave key 0 0 step ::
    translateNotes octave key mpt inverse_key notes
  | (Sharp (Step _) as step) :: notes ->
    translateNote octave key 0 0 step ::
    translateNotes octave key mpt inverse_key notes
  | (Sharp (Sharp (Step _)) as step) :: notes ->
    translateNote octave key 0 0 step ::
    translateNotes octave key mpt inverse_key notes
  | (Flat (Step _) as step) :: notes ->
    translateNote octave key 0 0 step ::
    translateNotes octave key mpt inverse_key notes
  | (Flat (Flat (Step _)) as step) :: notes ->
    translateNote octave key 0 0 step ::
    translateNotes octave key mpt inverse_key notes
  | note :: notes ->
    translateNotes octave key mpt inverse_key ((toStep note inverse_key) :: notes)
  | _ -> []

// Returns the absolute note from a (scale -> note) mapping
and toStep note inverse_key =
  let n = getOctave note in
    try
      get n (Map.find (get 0 note) inverse_key)
    with
      | _ -> get (n + 1) (Map.find (get 1 note) inverse_key)

// Returns the Root note from a diatonic mode key
and key2Note = function
  | Ionian     note -> note
  | Dorian     note -> note
  | Phrygian   note -> note
  | Lydian     note -> note
  | Mixolydian note -> note
  | Aeolian    note -> note
  | Locrian    note -> note

// Returns the octave for a declared music note
and getOctave = function
  | NoteA n     -> n
  | NoteB n     -> n
  | NoteC n     -> n
  | NoteD n     -> n
  | NoteE n     -> n
  | NoteF n     -> n
  | NoteG n     -> n
  | Step (_, n) -> n
  | Sharp note  -> getOctave note
  | Flat  note  -> getOctave note
  | _           -> terr "not the case ! - getOctave"

// Translates a musical phrase into absolute notes
and translatePhrase octave key mpt = function
  | (NotePhrase name) :: notes ->
    match (SymTab.Lookup name mpt) with
      | None             -> terr "Already handled in translatePhrase"
      | Some (phrase, _) ->
        translatePhrase octave key mpt phrase @
        translatePhrase octave key mpt notes
  | note :: notes ->
    betterNote (translateNote octave key 0 0 note) ::
    translatePhrase octave key mpt notes
  | []            -> []

// Translates an AbSyb.Note into a Music Note
and translateNote octave key sharps flats = function
  | NoteA n    -> N (A, n + octave, sharps, flats)
  | NoteB n    -> N (B, n + octave, sharps, flats)
  | NoteC n    -> N (C, n + octave, sharps, flats)
  | NoteD n    -> N (D, n + octave, sharps, flats)
  | NoteE n    -> N (E, n + octave, sharps, flats)
  | NoteF n    -> N (F, n + octave, sharps, flats)
  | NoteG n    -> N (G, n + octave, sharps, flats)
  | Sharp note -> translateNote octave key (sharps + 1) flats note
  | Flat  note -> translateNote octave key sharps (flats + 1) note
  | Step(n, p) when n >= 8 ->
    translateNote octave key sharps flats (Step (n % 8 + 1, p + n / 8))
  | Step(n, p) ->
    let oct  = octave + p
    let step = Map.find (Step (n, 0)) key
    betterNote (translateNote oct key sharps flats step)
  | _ -> terr "not the case ! - translateNote"

// Not nessesary, but a fair optimization:
// A lookup for double #'s or b's - translating into something that
// is more suitable for human reading (and printing)
and betterNote = function
  | N (A, n, ss, bs) when ss >= 2 -> betterNote (N (B, n, ss - 2, bs))
  | N (B, n, ss, bs) when ss >= 1 -> betterNote (N (C, n, ss - 1, bs))
  | N (C, n, ss, bs) when ss >= 2 -> betterNote (N (D, n, ss - 2, bs))
  | N (D, n, ss, bs) when ss >= 2 -> betterNote (N (E, n, ss - 2, bs))
  | N (E, n, ss, bs) when ss >= 1 -> betterNote (N (F, n, ss - 1, bs))
  | N (F, n, ss, bs) when ss >= 2 -> betterNote (N (G, n, ss - 2, bs))
  | N (G, n, ss, bs) when ss >= 2 -> betterNote (N (A, n, ss - 2, bs))
  | N (A, n, ss, bs) when bs >= 2 -> betterNote (N (G, n, ss, bs - 2))
  | N (B, n, ss, bs) when bs >= 2 -> betterNote (N (A, n, ss, bs - 2))
  | N (C, n, ss, bs) when bs >= 1 -> betterNote (N (B, n, ss, bs - 1))
  | N (D, n, ss, bs) when bs >= 2 -> betterNote (N (C, n, ss, bs - 2))
  | N (E, n, ss, bs) when bs >= 2 -> betterNote (N (D, n, ss, bs - 2))
  | N (F, n, ss, bs) when bs >= 1 -> betterNote (N (E, n, ss, bs - 1))
  | N (G, n, ss, bs) when bs >= 2 -> betterNote (N (F, n, ss, bs - 2))
  | N (h, n,  1,  1)              -> betterNote (N (h, n,  0,      0))
  | (note : MusicNote)          -> (note : MusicNote)

// Seems silly but, at some point we want this to collect tied measures as well.
and makeQBeats (bll : Beat list list) = List.map QB bll

// Translates rest notes into single fractions
and joinRests = function
  | b :: bs -> joinRest b :: joinRests bs
  | _       -> []

// Translate a rest note into a fraction, and leaves everything else untouched
and joinRest = function
  | R b' :: R b :: bs ->  joinRest (R (b' + b) :: bs)
  |           b :: bs ->  b :: joinRest bs
  | _                 -> []

// Check that the time signature and the beat fractions add up
and checkSum rythm c (Sig (n, d)) = function
  | []      -> true
  | m :: ms ->
    if   Q.Frac (n, d) = sum m
    then checkSum rythm (c + 1) (Sig (n, d)) ms
    else
      let (n', d') = Q.ToPair (sum m)
      verr (sprintf "The Rythm '%s' does not satisfy the time signature '%d/%d', as it adds upto '%d/%d' " rythm n d n' d') (-2, c)

and sum = function
  | H m :: [] -> m
  | R m :: [] -> m
  | H m :: ms -> m + sum ms
  | R m :: ms -> m + sum ms
  | _         -> terr "trying to sum the empty measure !"

// Deduces phrase- and midi-instrument lookup tables from a list of declarations
let verifyPhrases declarations =
  getPhrases
   (SymTab.Empty() : rphrase_table)
   (SymTab.Empty() : mphrase_table)
   (SymTab.Empty() : midi_table)
   (SymTab.Empty() : name_table)
   declarations

// Deduces rythm- and melody lookup tables from a list of declarations
let verifyPieces declarations phrases =
  getPieces
   (SymTab.Empty () : rythm_table)
   (SymTab.Empty () : melody_table)
   phrases
   declarations

let collectCompositions declarations =
  let phrases = verifyPhrases declarations
  let pieces  = verifyPieces  declarations phrases
  let rec collect compt  = function
    | CompositionDeclaration (name, comp, pos) :: xs ->
      collect (SymTab.Bind name (comp, pos) compt) xs
    | (_ :: xs) -> collect compt xs
    | _         -> compt
  let compositionTable =
    collect (SymTab.Empty () : composition) declarations in
  (compositionTable, pieces, phrases)

// and translateNote octave key sharps flats = function
let test declarations =
  printfn "%A" (collectCompositions declarations)
  0
