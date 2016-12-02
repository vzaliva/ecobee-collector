(* Extension of Getopt module which allows automatic generation of usage message from user-provided options' description.

Vadim Zaliva <lord@crocodile.org>
 *)

open Getopt
open List

(* extension of Getopt.opt with extra description field *)
type optext = char * string * ((unit -> unit) option) * ((string -> unit) option) * string

(* Special action function which will trigger printing usage. Specify it as 'action' field in 'optext' *)
let usage_action () = ()
                        
let optext2opt print_usage_func = function
  | (s,l,Some a,f2,_) ->
     if a == (usage_action) then
       (s,l,Some (fun _ -> Lazy.force print_usage_func), f2)
     else
       (s,l,Some a,f2)
  | (s,l,f1,f2,_) -> (s,l,f1,f2)

let get_usage eopts =
  let eopt_descr = function
    | (s, l, _, _, _) when s=noshort && l=nolong ->
       raise (Getopt.Error "Invalid option: must have either long or short form or both")
    | (s, l, _, None, d) when s=noshort -> Printf.sprintf "--%s :\t%s" l d
    | (s, l, _, None, d) when l=nolong -> Printf.sprintf "-%c :\t%s" s d
    | (s, l, _, None, d) -> Printf.sprintf "-%c, --%s :\t%s" s l d
    | (s, l, _, Some _, d) when s=noshort -> Printf.sprintf "--%s=<arg> :\t%s" l d
    | (s, l, _, Some _, d) when l=nolong -> Printf.sprintf "-%c <arg> :\t%s" s d
    | (s, l, _, Some _, d) -> Printf.sprintf "-%c <arg>, --%s=<arg> :\t%s" s l d
  in "Usage:\n\t" ^
       String.concat "\n\t" (map eopt_descr eopts)

(* This function could be manually invoked to print usage *)
let print_usage eopts = print_endline (get_usage eopts)

(* This handler for 'usage_action' will print usage when invoked when user specified appriate option*)
let print_usage_action eopts = (lazy (print_usage eopts))

(* This handler for 'usage_action' will print usage and exit with exit code 1 when invoked when user specified appriate option*)
let print_usage_and_exit_action eopts = (lazy (print_usage eopts; exit 1))

(* high-level interface to parse command line *)                                          
let ext_parse_cmdline eopts others usage =
  parse_cmdline
    (map (optext2opt (usage eopts)) eopts) others
    
    
