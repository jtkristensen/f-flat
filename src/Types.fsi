
module Types (* Some usefull types for the project *)

// A rational number is a pair of integers with operations '+' and '*'
// defined opun them!
type Q =
  F of int * int
  with
    static member ( + )  : Q * Q -> Q
    static member ( * )  : Q * Q -> Q
    static member ( / )  : Q * Q -> Q
    static member Frac   : int * int -> Q
    static member ToPair : Q -> int * int
    static member LCD    : Q * Q -> int
    static member Extend : Q -> int -> Q

// A single beat in a rythm (hit or rest)
type Beat =
  H of Q | R of Q
  with
    static member ( + )  : Beat * Beat -> Beat
    static member ( * )  : Beat * Beat -> Beat
    static member LCD    : Beat * Beat -> int
    static member Extend : int -> Beat -> Beat

// A word game. Its a Que of Beats containing numbers from Q.
// Iiiiitss QBeats ! {^o^}
type QBeats = QB of Beat list | T of QBeats * QBeats

// Musical notes, abstracted away from scales
// (so we can cast around the scale notes and leave the absolute ones be)
type Tone         = A | B | C | D | E | F | G
type AbsoluteNote = Tone * int * int * int
type ScaleStep    =  int * int * int * int
type MusicNote    = N of AbsoluteNote | S of ScaleStep
type Chanel       = (AbsoluteNote * Beat) list

// A (graph) vertex is a list of names of other vertecies (edges)
// and it can be marked a number of times (one for grey, two for black etc)
type Vertex =
  V of string list | M of Vertex
  with
    static member OfList : string list -> Vertex

// A graph is a consists of a list of vertecies with their associated names
type Graph  =
  Gr of SymTab.Table<Vertex>
  with
    static member DebthFirstSearch : Graph -> string list
    static member OfList : (string * Vertex) list -> Graph


// When searching for variable dependencies a "graph" can throw :
exception UnknownVariableName of string
exception CyclicReference     of string list
