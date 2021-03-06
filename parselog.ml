open Core
open String
open Str
open Printf

let debug_regexp = regexp "^Debug: ?\\([0-9]+\\(\\.[0-9]+\\)*\\) ?: *"

let verbose = ref false
let debug = ref false
let nofail = ref false
let ifname = ref ""
let ofname = ref ""

exception MissingArg of string

type kind =
  | Goal
  | Looking
  | SimpleApply of (bool*string)
  | SimpleEapply of (bool*string)
  | External of (bool*string)
  | NoMatch
  | Exact of bool*string
  | Unknown

let ok_fail_of_bool = function
  | true -> ""
  | false -> "[Fail]"

let string_of_kind = function
  | Looking            -> "Looking"
  | SimpleApply (x,n)  -> ok_fail_of_bool x ^ "SimpleApply " ^ n
  | SimpleEapply (x,n) -> ok_fail_of_bool x ^ "SimpleEapply " ^ n
  | External (x,n)     -> ok_fail_of_bool x ^ "External " ^ n
  | NoMatch            -> "NoMatch"
  | Exact (x,n)        -> ok_fail_of_bool x ^ "Exact " ^ n
  | Goal               -> "Goal"
  | Unknown            -> "???"

let html_of_kind = function
  | Looking            -> "<b>Looking</b>"
  | SimpleApply (x,n)  -> ok_fail_of_bool x ^ "<b>SimpleApply</b> <i>" ^ n ^ "</i>"
  | SimpleEapply (x,n) -> ok_fail_of_bool x ^ "<b>SimpleEapply</b> <i>" ^ n ^ "</i>"
  | External (x,n)     -> ok_fail_of_bool x ^ "<b>External</b> <i>" ^ n ^ "</i>"
  | NoMatch            -> "<b>NoMatch</b>"
  | Exact (x,n)        -> ok_fail_of_bool x ^ "<b>Exact</b> <i>" ^ n ^ "</i>"
  | Goal               -> "<b>Goal</b>"
  | Unknown            -> "<b>???</b>"

let is_err = function
  | Looking | NoMatch | Goal | Unknown -> false
  | SimpleApply (x,_) | SimpleEapply (x,_) | External (x,_) | Exact (x,_)  -> not x

let classifiers = [
    (regexp "^looking for (\\(.+\\)) with", fun _ -> Looking) ;
    (regexp "^simple apply \\([^ ]+\\) .*failed with",
     fun l -> SimpleApply (false, matched_group 1 l)) ;
    (regexp "^simple apply \\([^ ]+\\) on",
     fun l -> SimpleApply (true, matched_group 1 l)) ;
    (regexp "^simple eapply \\([^ ]+\\) .*failed with",
     fun l -> SimpleEapply (false, matched_group 1 l)) ;
    (regexp "^simple eapply \\([^ ]+\\)",
     fun l -> SimpleEapply (true, matched_group 1 l)) ;
    (regexp "^(\\*external\\*) \\([^ ]+\\) .*failed with",
     fun l -> External (false, matched_group 1 l)) ;
    (regexp "^(\\*external\\*) \\([^ ]+\\) on",
     fun l -> External (true, matched_group 1 l)) ;
    (regexp "^no match for",
     fun _ -> NoMatch) ;
    (regexp "^(",
     fun _ -> Goal) ;
    (regexp "^exact \\([^ ]+\\) .*failed with",
     fun l -> Exact (false, matched_group 1 l)) ;
    (regexp "^exact \\([^ ]+\\) on",
     fun l -> Exact (true,matched_group 1 l)) ;
  ]

let classify l =
  let rec loop = function
    | [] -> Unknown
    | (r,c)::rs -> if string_match r l 0 then (c l) else loop rs
  in
  loop classifiers

(* Seqence type and functions *)

type seq = int list

let string_of_seq l = concat ~sep:"." (List.map ~f:string_of_int l)

let seq_of_string s = List.map ~f:int_of_string (String.split s ~on:'.')

let rec ( =@ ) a b =
  match a, b with
  | (x::xs), (y::ys) -> (Int.(=) x y) && xs=@ys
  | [], [] -> true
  | [], (_::_) -> false
  | (_::_), [] -> false

(* True if 'h' starts with 'n', but not exactly the same *)
let rec ( >@ ) h n =
  match h, n with
  | (h::hs), (n::ns) -> (Int.(=) h n) && hs >@ ns
  | [], [] -> false
  | [], (_::_) -> false
  | (_::_), [] -> true

(* Stack of states for DFS *)
type entry = {
    line: int;
    b: seq;
    kind: kind;
    msg: string
  }

let string_of_entry e =
  sprintf "%d:%s:%s:[%s]" e.line (string_of_seq e.b) (string_of_kind e.kind)
    (if !verbose then e.msg else "")

let stack:(entry Stack.t) = Stack.create ()

(* Find first (...) expression in string *)
let find_expr l =
  let b = index_exn l '(' in
  let e = rindex_exn l ')' in
  slice l b e

let rec html_escape s =
  if is_empty s then ""
  else
    (match s.[0] with
     | '\"' -> "&quot;"
     | '&' -> "&amp;"
     | '<' -> "&lt;"
     | '>' -> "&gt;"
     | x -> of_char x) ^ html_escape (String.drop_prefix s 1)

let html_expr l =
  let open String.Search_pattern in
  let p = create "\n" in
  replace_all p ~in_:(html_escape l) ~with_:"<br/>"

let gen_entry l n =
  let lflat = filter l ~f:(Char.(<>) '\n') in
  if string_match debug_regexp lflat 0 then
    let bs = matched_group 1 lflat in
    let me = match_end () in
    let m = string_after lflat me in
    let k = classify m in
    Some { line = n;
           b = seq_of_string bs;
           kind = k;
           msg = (if !verbose then
                    match k with
                    | Looking -> "<b>Looking for</b>:<br/><br/><i>"^ (html_expr (find_expr l)) ^"</i>"
                    | _ -> html_of_kind k
                  else
                    string_of_kind k
                 ) ;
      }
  else
    None

(* TODO: add styles *)
let dot_style_of_kind k =
  let col c = "[color=" ^ c ^ "]" in
  let sha s = "[shape=" ^ s ^ "]" in
  let errc c x = col (if x then c else "red") in
  match k with
  | Looking            -> sha "box" ^ col "black"
  | SimpleApply (x,_)  -> sha "box" ^ errc "blue" x
  | SimpleEapply (x,_) -> sha "box" ^ errc "blue" x
  | External (x,_)     -> sha "box" ^ errc "pink" x
  | NoMatch            -> sha "ellipse" ^ col "red"
  | Exact (x,_)        -> sha "box" ^ errc "green" x
  | Goal               -> sha "polygon" ^ col "yellow"
  | Unknown            -> sha "doublecircle" ^ col "red"

let dot_of_entry {line; b; kind; msg} =
  let bs = string_of_seq b in
  sprintf "L%d %s [label=<%s<br/>%s%s>]"
    line
    (dot_style_of_kind kind)
    bs
    (if !debug then (string_of_int line) ^ ":" else "")
    msg

let rec dump_dot oc msibling prev =
  let link a mb = match mb with
    | Some b -> fprintf oc "\tL%d -> L%d;\n" a.line b.line;
    | None -> ()
  in
  if not (Stack.is_empty stack) then
    let p = Stack.top_exn stack in
    if
      (match msibling with
       | Some sibling -> sibling.b >@ p.b
       | None -> false)
    then
      (* we are at common parent. just link from it *)
      link p prev
    else
      begin
        let x = Stack.pop_exn stack in
        (* if !debug then printf "\t\tPOP %s\n" (string_of_entry x); *)
        fprintf oc "\t%s;\n" (dot_of_entry x);
        link x prev;
        dump_dot oc msibling (Some p)
      end

let process_line oc l n =
  match gen_entry l n with
  | Some e ->
     printf "%s\n" (string_of_entry e);
     if not (phys_equal e.kind Unknown)
        && not (phys_equal e.kind Goal)
        && (not !nofail || not (is_err e.kind))
     then
       if is_err e.kind
          && not (Stack.is_empty stack)
          && (Stack.top_exn stack).b =@ e.b
       then
         begin
           (* Fold multiple fail entries into one box *)
           let p = Stack.pop_exn stack in
           Stack.push stack {e with msg = p.msg ^ "<br/>" ^
                                      (if !debug then (string_of_int e.line) ^ ":" else "") ^
                                        e.msg
             }
         end
       else
         begin
           dump_dot oc (Some e) None;
           Stack.push stack e;
         end
  (* if !debug then printf "\t\tPUSH: %s, stack size %d\n" (string_of_seq e.b) (Stack.length stack) *)
  | None ->
     if !debug && !verbose then printf "Not numbered: %d: %s\n" n l

let process_file ifilename ofilename =
  let ic = In_channel.create ifilename in
  let oc = Out_channel.create ofilename in
  let rec loop m start current =
    let s = In_channel.input_line_exn ic in
    if not (is_empty m) && String.is_prefix s ~prefix:"Debug" then
      begin
        process_line oc m start ;
        loop s current (current+1)
      end
    else
      loop (m ^ "\n" ^ s) start (current+1)
  in
  try
    fprintf oc "digraph {\n" ;
    fprintf oc "\trankdir=\"LR\";\n" ;
    loop "" 1 1
  with End_of_file ->
    begin
      (* TODO: dump remaining stack *)
      dump_dot oc None None ;
      fprintf oc "}\n" ;
      In_channel.close ic ;
      Out_channel.close oc
    end

let main =
  begin
    let speclist = [("-v", Arg.Set verbose, "Enables verbose mode");
                    ("-d", Arg.Set debug, "Enables debug mode");
                    ("-x", Arg.Set nofail, "Exclude failed applications");
                    ("-f", Arg.Set_string ifname, "File to process");
                    ("-o", Arg.Set_string ofname, "Output file");
                   ]
    in let usage_msg = "Parse log file. Options available:"
       in Arg.parse speclist print_endline usage_msg;
          if is_empty !ifname then raise (MissingArg "Must specify -f") ;
          if is_empty !ofname then raise (MissingArg "Must specify -o") ;
          process_file !ifname !ofname
  end

let () = main
