open Netkat_netkat
open Stdlib

(* Debugging aid: parse a .nkpl file and, for each command, print its
   normalized (pretty-printed) form followed by its raw AST. *)

let quote s = "\"" ^ String.escaped s ^ "\""
let field f = quote (Field.get_or_fail_fid f)
let opt f = function None -> "None" | Some x -> "Some (" ^ f x ^ ")"

(* Constructor-level dump of an [Nkexp.t]; unlike [Nkexp.to_sexp] this keeps
   every argument (e.g. the optional start set of Fwd/Bwd). *)
let rec exp_ast ind (e : Nkexp.t) =
  let pad = String.make ind ' ' in
  let node name kids = pad ^ name ^ "\n" ^ String.concat "\n" (List.map (exp_ast (ind + 2)) kids) in
  let leaf s = pad ^ s in
  match e with
  | Drop -> leaf "Drop"
  | Skip -> leaf "Skip"
  | Dup -> leaf "Dup"
  | Filter (b, f, v) -> leaf (Printf.sprintf "Filter (%b, %s, %s)" b (field f) (Value.to_string v))
  | Mod (f, v) -> leaf (Printf.sprintf "Mod (%s, %s)" (field f) (Value.to_string v))
  | VFilter (b, f, x) -> leaf (Printf.sprintf "VFilter (%b, %s, %s)" b (field f) x)
  | VMod (f, x) -> leaf (Printf.sprintf "VMod (%s, %s)" (field f) x)
  | Seq l -> node "Seq" l
  | Union l -> node "Union" l
  | Intersect l -> node "Intersect" l
  | Star e -> node "Star" [ e ]
  | Diff (a, b) -> node "Diff" [ a; b ]
  | Xor (a, b) -> node "Xor" [ a; b ]
  | Neg e -> node "Neg" [ e ]
  | Fwd (None, e) -> node "Fwd" [ e ]
  | Fwd (Some p, e) -> node "Fwd (with start)" [ p; e ]
  | Bwd (None, e) -> node "Bwd" [ e ]
  | Bwd (Some p, e) -> node "Bwd (with start)" [ p; e ]
  | Exists (f, e) -> node ("Exists " ^ field f) [ e ]
  | Forall (f, e) -> node ("Forall " ^ field f) [ e ]
  | Var x -> leaf ("Var " ^ x)
  | Num n -> leaf ("Num " ^ string_of_int n)
  | Lambda (x, e) -> node ("Lambda " ^ x) [ e ]
  | App (a, b) -> node "App" [ a; b ]

let dmode_ast : Nkcmd.dmode -> string = function
  | DBestEffort -> "DBestEffort"
  | DExhaustive -> "DExhaustive"
  | DExplicit vs ->
    "DExplicit ["
    ^ String.concat "; "
        (List.map (function Nkcmd.Dnum v -> "Dnum " ^ Value.to_string v | Dvar s -> "Dvar " ^ s) vs)
    ^ "]"

let rec cmd_ast ind (c : Nkcmd.t) =
  let pad = String.make ind ' ' in
  let sub = exp_ast (ind + 2) in
  match c with
  | Import s -> pad ^ "Import " ^ quote s
  | Prints s -> pad ^ "Prints " ^ quote s
  | Print e -> pad ^ "Print\n" ^ sub e
  | Tikz e -> pad ^ "Tikz\n" ^ sub e
  | Rep e -> pad ^ "Rep\n" ^ sub e
  | Let (x, e) -> pad ^ "Let " ^ x ^ "\n" ^ sub e
  | VLet (x, v) -> pad ^ "VLet " ^ x ^ " = " ^ Value.to_string v
  | Check (tag, b, e1, e2) ->
    Printf.sprintf "%sCheck (tag=%s, equiv=%b)\n%s\n%s" pad (opt quote tag) b (sub e1) (sub e2)
  | Simulate (tag, n, fields, start, e) ->
    let fs =
      String.concat "; " (List.map (fun (f, m) -> field f ^ " -> " ^ dmode_ast m) fields)
    in
    Printf.sprintf "%sSimulate (tag=%s, n=%s, fields=[%s])\n%s  start: %s\n%s" pad (opt quote tag)
      (opt string_of_int n) fs pad
      (match start with None -> "None" | Some s -> "\n" ^ exp_ast (ind + 4) s)
      (sub e)
  | For (x, lo, hi, body) -> Printf.sprintf "%sFor %s = %d..%d\n%s" pad x lo hi (cmd_ast (ind + 2) body)

let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "usage: netkat-ast <nkpl-file>";
    exit 2);
  let cmds = Interp.parse_file print_string Sys.argv.(1) in
  List.iteri
    (fun i c ->
      Printf.printf "=== command %d ===\n" (i + 1);
      Printf.printf "-- normalized --\n%s\n" (Nkcmd.to_string c);
      Printf.printf "-- AST --\n%s\n\n" (cmd_ast 0 c))
    cmds
