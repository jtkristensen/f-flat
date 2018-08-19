
module MIDI

open System.IO
open Types
open AbSyn

let note2pitch = function
  | (C, octave, sharps, flats) -> 60 + (12 * octave) + sharps - flats
  | (D, octave, sharps, flats) -> 62 + (12 * octave) + sharps - flats
  | (E, octave, sharps, flats) -> 64 + (12 * octave) + sharps - flats
  | (F, octave, sharps, flats) -> 65 + (12 * octave) + sharps - flats
  | (G, octave, sharps, flats) -> 67 + (12 * octave) + sharps - flats
  | (A, octave, sharps, flats) -> 69 + (12 * octave) + sharps - flats
  | (B, octave, sharps, flats) -> 71 + (12 * octave) + sharps - flats

let makePitch (i, s, l, ns) =
  (i, s, l, List.map (fun (note, beat) -> (note2pitch note, beat)) ns)

let smallestSub b0 b1 = H (Q.Frac (1, Beat.LCD (b0, b1)))

let rec findSmallest smallest = function
  | beat :: beats -> findSmallest (smallestSub smallest beat) beats
  | _             -> smallest

let rec extendBeats q = function
  | beat :: beats ->
    let (_, d) = Q.ToPair q in (Beat.Extend d beat) :: extendBeats q beats
  | _             -> []

let rec smallestInChannels q = function
  | (_, _, _, channel) :: channels ->
    let p = findSmallest q ((List.unzip >> snd) channel)
    in smallestInChannels p channels
  | _ -> q

let rec smallestInTrack =
  let rec f q = function
    | channels :: track -> let p = smallestInChannels q channels in f p track
    | _                 -> q
  f (H (Q.Frac (1, 1)))

let transform track =
  let smallest = match smallestInTrack track with | H q -> q | R q -> q
  let rec f m = function
    | channels :: track -> g m channels :: f m track
    | _                 -> []
  and     g m = function
    | []                   -> []
    | (i, s, b, bs) :: bss ->
      let (tones, beats) = List.unzip bs in
      makePitch ((i, s, b, List.zip tones (extendBeats m beats))) :: g m bss
  (f smallest track,
   (fst << Q.ToPair) (Q.Extend (Q.Frac (1, 4)) ((snd << Q.ToPair) smallest)))

let getFrac b = match b with | H n -> n | R n -> n

let rec getNotes ch t vol =
  let line = sprintf "mf.addNote (%5d, %5d, %5d, %5d, %5d, %5d)\n" 0
  function
    | []                     -> ("", t)
    | (pitch, beat) :: notes ->
      match beat with
        | R q ->
          let (n, _) = Q.ToPair q in getNotes ch (t + n) vol notes
        | H q ->
          let (n, _)   = Q.ToPair q in
          let (n', t') = (getNotes ch (t + n) vol notes) in (line ch pitch t n vol + n', t')

let rec getChannels sch it nt time =
  let channel = sprintf "\nmf.addProgramChange (%5d, %5d, %5d, %5d)\n" 0
  let rec f ch t t' = function
    | []                                          -> ("", t')
    | (InstrumentName inst, sign, _, notes) :: cs ->
      match SymTab.Lookup inst it with
        | None             -> Compose.nerr inst nt "musical instrument"
        | Some (sfid, vol, _) ->
          let (n' , t'') = getNotes ch t vol notes
          let (ns , tt ) = f (ch + 1) t (max t' t'') cs
          (channel ch t sfid + n' + ns, max t tt)
  f sch time time

let rec miditrack it nt time = function
  | []                   -> ""
  | channels :: track    ->
    let (txt, time') = getChannels 0 it nt time channels in
    txt + miditrack it nt time' track

// Each channel is a list of notes and beats.
// The track consists of a number of simulaniously playing channels
let getPymidi tempo title (track, instruments, names) =
  let _ = (track       : (MidiInstrument * Signature * Beat *
                          (AbsoluteNote * Beat) list) list list)
  let _ = (instruments : SymTab.Table<int * int * Position>)
  let midi_track, quatertime = transform track
  "from midiutil.MidiFile import MIDIFile\n" +
  "mf = MIDIFile(1)\n" +
  (sprintf "mf.addTempo(0, 0, %d)\n" (quatertime * tempo)) +
  (miditrack instruments names 0 midi_track) +
  "with open('"+title+".mid', 'wb') as outf:\n" +
  "    mf.writeFile(outf)\n"
