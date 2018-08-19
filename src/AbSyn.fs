
module AbSyn (* Various constructs for the abstract syntax tree *)

// For reasonable error messages, every type should in time be extended with this
type Position = int * int

// A rythmic note is a note with no information about pitch
type RythmNote =
  | RNote    of int            // denominator of the
  | Rest     of RythmNote
  | Flagged  of RythmNote
  | Dotted   of RythmNote
  | Triplet  of RythmNote
  | Tie      of RythmNote list
  | PhraseID of string
  | Beam     of RythmNote list

// Measures consists of rythmic notes
type Measure =
  | Single   of RythmNote list    // no tie
  | Tied     of Measure * Measure // two measures with a tie

// Musical notes can be sharp or flat, futhermore, they can be "generic",
// meaning that we reprecent them by their ID into a diatonic scale rahter
// than by their absolute value.
type Note =
  | NoteA of int | NoteB of int | NoteC of int | NoteD of int
  | NoteE of int | NoteF of int | NoteG of int
  | Step       of int * int // scale step, pitch
  | NotePhrase of string    // a generic melodic phrase of notes
  | Sharp      of Note
  | Flat       of Note

// The standard diatonic modes, combined with a rootnote makes up a
// key signature.
type Key =
  | Ionian     of Note
  | Dorian     of Note
  | Phrygian   of Note
  | Lydian     of Note
  | Mixolydian of Note
  | Aeolian    of Note
  | Locrian    of Note

// Time signature (fraction)
type Signature = Sig of int * int

// The instruments in a MIDI sound font, is denoted by a number.
// We allow instruments to be accessed directly by this number,
// or to be declared as a variable (referencing a number)
type MidiInstrument =
  | InstrumentName of string

// A melody is merely a list of notes played be a certain instrument
// in a certain key.

// Complicated phrasing is a new way of combining ryhtms {~_^}
type Phrasing =
  | Simple       of string
  | Repetition   of Phrasing * int
  | Union        of Phrasing * Phrasing
  | Intersection of Phrasing * Phrasing
  | Subtraction  of Phrasing * Phrasing
  | Projection   of Phrasing * string
  | RCast        of Phrasing * Signature

// A composition is a combination of phrases, melodies and rythms
// TODO : This belongs somewhere else!
type Composition =
  | Var          of string
  | Simultanious of Composition * Composition
  | Assignment   of Composition * Phrasing
  | Before       of Composition * Composition
  | After        of Composition * Composition
  | MCast        of string * Key

// The parser will return a list of Decalrations which we will check
// for correctness and split
type UnchekedDeclaration =
  | Beat                   of string * Signature *       Measure list * Position
  | Repeat                 of string * Signature * int * Measure list * Position
  | MelodicPhrase          of string * Note list       * Position
  | RythmicPhrase          of string * RythmNote list  * Position
  | ComposedMelody         of string * MidiInstrument  * Key * (Note list) * Position
  | InstrumentDeclaration  of string * int * int       * Position
  | CompositionDeclaration of string * Composition     * Position
