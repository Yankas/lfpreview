type event = | Left | Right | Middle | Update

(************************
    Utility Functions 
************************)
let run_read_lines prog : (string list, string) result =
    let lines = ref [] in
        let in_channel = Unix.open_process_in prog in
    begin
        try
            while true do lines := input_line in_channel :: !lines done;
        with End_of_file -> ignore (Unix.close_process_in in_channel)
    end;    
    Ok(List.rev !lines)

let run_read prog result = 
    match run_read_lines prog with
    | Ok l -> Ok(String.concat "\n" l)
    | Error x -> Error("ERROR: command '" ^ prog ^ "' failed.")

let regex_first_capture regex str = 
    let regexp = Str.regexp regex in
    try
        Str.search_forward regexp str 0 |> ignore;
        Some(Str.matched_group 1 str)
    with
    | Not_found -> None

let ( |>> ) v f  = match v with | Ok _ -> f v | Error x -> x    
let open_status_win cmd = 
    Unix.system (Printf.sprintf "kitty --name popup-status --hold %s" cmd) |> ignore 

let try_command cmd handler = 
    match cmd with | Ok a -> handler a | Error _ -> "ERR"

let on event_match action event  = if event = event_match then action |> ignore  

(************************
    Widget Implementation
************************)
let usd _ = 
    Some(
        match run_read "curl -Ss http://www.floatrates.com/daily/usd.json" with
        | Ok std_out ->
            (match (regex_first_capture "\"rate\":\\([0-9]+\\.[0-9][0-9]\\)" std_out) with
            | Some x -> x |> ( ^ ) "💲"
            | None -> "ERROR")
        | Error _ -> "ERR")

let volume event = 
    let out = 
        try_command (run_read "pactl get-sink-volume @DEFAULT_SINK@") 
        (fun std_out ->
            match regex_first_capture "\\([0-9]*\\)%" std_out with
            | Some v -> (match int_of_string v with
                | vol when (vol > 66) -> "🔊 " ^ (string_of_int vol) ^ "%"
                | vol when (vol > 33) -> "🔉 " ^ (string_of_int vol) ^ "%"
                | vol when (vol > 0) -> "🔈 " ^ (string_of_int vol) ^ "%"
                | _ -> "🔇")
            | None -> "ERR")
    in
    match event with 
    | Left -> Unix.system "pavucontrol" |> ignore; Some(out)
    | Right | Update | Middle -> Some(out)

let date event =
    let time = Unix.time () |> Unix.localtime in
    let out = Some (Printf.sprintf "🗓️%02i/%02i/%02i" (time.tm_mon + 1) time.tm_mday (time.tm_year mod 100)) in
    match event with
        | Left -> Unix.system "i3-msg -q [instance=\"popup-cale\"] scratchpad show && i3-msg -q [instance=\"popup-cale\"] move position center"  |> ignore; out
        | _ -> out

let updates _ =
    Some(try_command (run_read "checkupdates | wc -l")
            (function | "" | "0" -> "" | x -> "🚩" ^ x))

let time event =
    let time = Unix.time () |> Unix.localtime in
    let out = Some (Printf.sprintf "🕑%02i:%02i:%02i" time.tm_hour time.tm_min time.tm_sec) in
    match event with
        | Left -> Unix.system "i3-msg -q [instance=\"popup-cale\"] scratchpad show && i3-msg -q [instance=\"popup-cale\"] move position center"  |> ignore; out
        | _ -> out
        
let weather event =
    let out = 
        match run_read_lines "curl -Ss \"https://wttr.in/mitte?u&format=%c%t\\n\"" with
        | Ok lines -> Some (List.hd lines)
        | Error _ -> Some "hi"
    in
    match event with 
    | Left -> open_status_win "curl -Ss \"https://wttr.in/mitte?F\""; out
    | _ -> out


(************************
    Program execution
************************)
let () = 
    let event =     
        match Sys.getenv_opt "button" with 
        | Some "1" -> Left
        | Some "2" -> Middle
        | Some "3" -> Right
        | Some _ | None -> Update
    in
    match 
        begin
            match List.find_opt (fun widget -> Sys.argv.(1) = fst widget) [ "time", time; "date", date; "weather", weather; "volume", volume; "usd", usd; "updates", updates]
            with 
            | Some widget -> (snd widget) event
            | None -> Some (Printf.sprintf "\"%s\" not implemented." Sys.argv.(1)) 
        end
    with 
    | Some str -> Printf.printf "%s\n" str
    | None -> Printf.printf "FAILURE"
    |> ignore;
    exit 0