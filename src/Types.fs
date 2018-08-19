
module Types (* Implementation of the 'Types' Signature *)

// A naive implementation for rational numbers
type Q =
  F of int * int

let multQ ( F(a, b) ) = function
  | F(c, d) when a = b -> F(c, d)
  | F(c, d) when b = c -> F(a, d)
  | F(c, d) when a = d -> F(c, b)
  | F(c, d)            -> F(a * c, b * d)

let addQ ( F(a, b) ) = function
  | F(c, d) when b = d -> F(a + c, b)
  | F(c, d)            -> F(a*d + b*c , b*d)

let rec gcd = function
  | (a, 0) -> a
  | (a, b) -> gcd (b, a % b)

let multiples n = Seq.initInfinite (( + ) 1 >> ( * ) n)

let rec smallestEq (ns, ms) =
  match Seq.head ns, Seq.head ms with
  | (n, m) when n = m -> n
  | (n, m) when n < m -> smallestEq (Seq.tail ns, ms)
  | (n, m)            -> smallestEq (ns, Seq.tail ms)

let rec LeastCommonDivisor (F(_, d), F(_, d')) =
  smallestEq (multiples d, multiples d')

//     a/b * x = c / d
// <=>       x = (bc / ad)
let solveForX (F(a, b)) (F(c, d)) = F( b * c, a * d)

let reduceQ (F (a, b)) = let c = gcd (a, b) in F(a / c, b / c)

type Q with
  static member ( + )  (f0, f1)     = reduceQ (addQ  f0 f1)
  static member ( * )  (f0, f1)     = reduceQ (multQ f0 f1)
  static member ( / )  (f0, f1)     = solveForX f0 f1
  static member Frac   (a , b )     = reduceQ (F (a, b))
  static member ToPair (F (a, b))   = (a, b)
  static member LCD    (f0, f1)     = LeastCommonDivisor (f0, f1)
  static member Extend (F(n, d)) m  =
    match (m % d) with
      | 0  -> F(n * (m / d), m)
      | _  -> failwith (sprintf "%d does note divide %d" d m)

type Beat = H of Q | R of Q

let addBeat = function
  | H p, H q -> H (p + q)
  | R p, H q -> R (p + q)
  | H p, R q -> R (p + q)
  | R p, R q -> R (p + q)

let multBeat = function
  | H p, H q -> H (p + q)
  | R p, H q -> R (p + q)
  | H p, R q -> R (p + q)
  | R p, R q -> R (p + q)

let blcd = function
  | H p, H q -> Q.LCD (p, q)
  | R p, H q -> Q.LCD (p, q)
  | H p, R q -> Q.LCD (p, q)
  | R p, R q -> Q.LCD (p, q)

let extendBeat m = function
  | H p -> H (Q.Extend p m)
  | R p -> R (Q.Extend p m)

type Beat with
  static member ( + ) (b0, b1) = addBeat(b0, b1)
  static member ( * ) (b0, b1) = multBeat(b0, b1)
  static member LCD   (b0, b1) = blcd (b0, b1)
  static member Extend m b     = extendBeat m b

type QBeats       = QB of Beat list | T of QBeats * QBeats
type Octave       = int
type Sharps       = int
type Flats        = int
type Tone         = A | B | C | D | E | F | G
type AbsoluteNote = Tone * Octave * Sharps * Flats
type ScaleStep    =  int * Octave * Sharps * Flats
type MusicNote    = N of AbsoluteNote | S of ScaleStep
type Chanel       = (AbsoluteNote * Beat) list

// Actually it could be fun to have a small DSL for graph algorithms. {^_^}
// However, heres a naive implementation of graphs and verticies
type Vertex =
  | V of string list // the list is implicitly the edges
  | M of Vertex      // A marked vertex

let rec edges = function | V e -> e | M v -> edges v
and    marked = function | M _ -> true | _ -> false
and    unmark = function | M v -> unmark v | V e as v -> v

type Vertex with
  static member Mark   v = M v
  static member UnMark v = unmark v
  static member Marked v = marked v
  static member Edges  v = edges  v
  static member OfList v = V v

// For now a graph is just a Symbol table (since we already implemented that).
// In later versions of the compiler we can select a more optimal data structure.
type Graph =
  Gr of SymTab.Table<Vertex>

exception UnknownVariableName of string
exception CyclicReference     of string list

// Don't know if we are going to use all of these methods,
// but they are usefull for implementing algorithms from CLRS.
type Graph with
  static member Get (v : string) (Gr vs) =
    match (SymTab.Lookup v vs) with
      | Some v' -> v'
      | None    -> raise (UnknownVariableName v)
  static member IsMarked (v : string) (g : Graph) = marked (Graph.Get v g)
  static member UnmarkedList (Gr vs) =
    (List.filter (fun (n, v) -> not(marked(v)))) (SymTab.ToList vs)
  static member Mark (v : string) (Gr vs as g) =
    let v' = Graph.Get v g in Gr (SymTab.Bind v (M v') (SymTab.Remove v vs))
  static member Unmark (v : string) (Gr vs as g) =
    let v' = Graph.Get v g in Gr (SymTab.Bind v (unmark v') (SymTab.Remove v vs))
  static member SelectFirstUnmarkedFrom g =
      List.head (Graph.UnmarkedList g)
  static member ToList (Gr vs) = fst (List.unzip (SymTab.ToList vs))
  static member OfList vs = Gr (SymTab.OfList vs)

// The book example of DFS
// We use this for detecting cyclic variable references.
let rec debthFirstSearch ({contents = (Gr vs)} as g : Graph ref) =
  let l = ref []
  while (List.sort (Graph.ToList (!g))) <> List.sort (!l) do
    visit g l (fst (Graph.SelectFirstUnmarkedFrom (!g)))
  (!l)

and visit (g : Graph ref) (l : string list ref) (v : string) =
  match (Graph.IsMarked v (!g), List.exists ((=) v) (!l)) with
    | true , false -> raise (CyclicReference (!l))
    |     _, false ->
      g := Graph.Mark v (!g)
      for w in edges (Graph.Get v (!g)) do
        visit g l w
      l := v :: (!l)
    | _ -> () // Nothing to do for this node, we already visited it

type Graph with
  static member DebthFirstSearch g = debthFirstSearch (ref g)
