
module SymTab (* A polymorphic symbol table *)

// A symbol table is just a list for now
type Table<'a> = SymTab of (string * 'a) list

let Empty () = SymTab []

let rec Lookup n = function
  | SymTab []                 -> None
  | SymTab ( (n', i') :: st') ->
    if n = n' then Some i' else Lookup n (SymTab st')

let Bind n i (SymTab stab) = SymTab ( (n,i) :: stab)

let Remove n (SymTab stab) =
    SymTab (List.filter (fun (x, _) -> x <> n) stab)

let RemoveMany ns (SymTab stab) =
  SymTab (List.filter (fun (x, _) ->
            not (List.exists (fun y -> y = x) ns)) stab)

let Combine (SymTab t1) (SymTab t2) = SymTab (t1 @ t2)
let OfList l = SymTab l
let ToList (SymTab lst) = lst
