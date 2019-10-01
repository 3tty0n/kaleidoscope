type expr =
  (* variant for numeric variables like `1.0' *)
  | Number of float
  (* variant for referencing variabels *)
  | Variable of string
  (* variant for a binary operator. *)
  | Binary of char * expr * expr
  (* variant for function calling *)
  | Call of string * expr array


(* proto - This type represents the "prototype" for a function, which captures
 * its name, and its argument names (thus implicitly the number of arguments the
 * function takes). *)
type proto = Prototype of string * string array

(* func - This type represents a function definition itself. *)
type func = Function of proto * expr

type toplevel =
  | Expression of expr
  | Extern of proto
  | Definition of func
  | Sep
  | End
