open Getopt
open Getoptext
open Yojson.Basic.Util

let prograname = "EcobeeCollector" (* must match executable module name *)
and version = "0.1"
and default_cfgfile  = "collector.cfg"
and default_logfile  = "collector.log"
                         
and debug = ref false
and cfgfile  = ref ""
and logfile  = ref ""

let specs = 
  [
    ( 'v', "version", Some (fun _ -> Printf.printf "%s %s\n" prograname version ; exit 0), None,
      "Show program version");
    ( 'h', "help", Some usage_action, None,
      "Show this help");
    ('c', "console",  Some (fun _ -> logfile := "<stderr>"), None,
     "Log to console instead of log file");
    ( 'd', "debug", (set debug true), None,
      "Debug");
    ( 'f', "config",  None, (atmost_once cfgfile (Error "only one config")),
      (Printf.sprintf "config file name. Default (%s)" default_cfgfile));
    ( 'l', "log",  None, (atmost_once logfile (Error "only one log")),
      (Printf.sprintf "log file name. Default (%s)" default_logfile))
  ]

let read_cfg () =
  LOG "Reading config from '%s'" !cfgfile LEVEL DEBUG;
  (* Check for presence of required fields *)
  let c = Yojson.Basic.from_file !cfgfile in
  match
    (c |> member "client-id" |> to_option to_string),
    (c |> member "refresh-token" |> to_option to_string),
    (c |> member "interval" |> to_option to_int),
    (c |> member "attempts" |> to_option to_int)
  with
  | Some _, Some _, Some _, Some _ -> c
  | None, _, _ , _ -> LOG "Missing 'client-id' config value" LEVEL ERROR; exit 1
  | _, None, _ , _ -> LOG "Missing 'refresh-token' config value" LEVEL ERROR; exit 1
  | _, _, None, _ -> LOG "Missing 'interval' config value" LEVEL ERROR; exit 1
  | _, _, _, None -> LOG "Missing 'attempts' config value" LEVEL ERROR; exit 1
                                                                           

let setup_log () =
  let seconds24h = 86400. in
  let dt_layout = Bolt.Layout.pattern
                    [] [] "$(year)-$(month)-$(mday) $(hour):$(min):$(sec) $(level:5): $(message)" in
  Bolt.Layout.register "datetime" dt_layout ;
  Bolt.Logger.register
    prograname
    (if !debug then Bolt.Level.TRACE else Bolt.Level.INFO)
    "all"
    "datetime"
    (Bolt.Mode.direct ())    
    "file" (!logfile, ({Bolt.Output.seconds_elapsed=Some seconds24h; Bolt.Output.signal_caught=None}))

let parse_cmdline () =
  let ue _ = print_usage specs; exit 1 in
  (try ext_parse_cmdline specs ue print_usage_and_exit_action with
   | Getopt.Error s -> Printf.printf "Error:\n    %s\n" s; ue ());
  if !cfgfile = "" then cfgfile := default_cfgfile;
  if !logfile = "" then logfile := default_logfile

let _ =
  parse_cmdline ();
  setup_log ();
  LOG "Launched" LEVEL INFO;

  let c = read_cfg () in
  let cliend_id = c |> member "client-id" |> to_string in
  let refresh_token = c |> member "refresh-token" |> to_string in
  let interval = c |> member "interval" |> to_int in
  let attempts = c |> member "attempts" |> to_int in
  
  let rec mainloop () =
    let rec try_fetch a =
      let rc = 0 in (* TODO: fetch here *)
      if rc=0 then
        (LOG "Fetch OK" LEVEL DEBUG; rc)
      else
        (LOG "Fetch attempt %d failed with code %d" (attempts-a+2) rc LEVEL DEBUG;
         if a=0 then rc else try_fetch (a-1))
    in
    let rc = try_fetch (attempts+1) in
    (* TODO: process fetch results if rc=0 *)
    Unix.sleep interval;
    mainloop ()
  in mainloop ()
