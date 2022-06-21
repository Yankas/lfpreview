(*
* This program provides file previews for the lf file manager.

* ARGUMENTS
** if 5 arguments are supplied
[1] = file name to be previewed
[2] = available preview width in cells
[3] = available preview height in cells
[4] = horizontal position of the preview window
[5] = vertical position of the preview window

** if 1 argument is supplied
[1] = file name to be cleaned

* OUTPUT
Preview in text format that represents the preview of the input file ARGS[0]

* DEPENDENCIES
highlight - syntax highlighting for text files 
kitty - terminal with image support
ffmpegthumbnailer - preview for video files
libcdio - for isoinfo (preview of iso files)
docx2txt - for MS Word docx Preview
odt2txt - for OpenDocument (ODT) preview
exiftool - command exiftool for audio metadata preview (arch-package: perl-image-exiftool)
catdoc - Preview for .doc files 
poppler - Contains 'pdftoppm' command needed to converting PDFs to images.

* LF documentation
https://pkg.go.dev/github.com/gokcehan/lf#hdr-Configuration
*)
open Rules

type position = {  width : string; height: string; left: string; top: string  }
type mime_info = { main: string; sub: string; parameter: string option }
type file_info = { path: string; mime: mime_info; }
type file_handler = (file_info -> position -> (int, string) result)
type match_condition = | Extension of string list | Mime of (mime_info -> bool)
type rule = { condition: match_condition; action: file_handler }
exception CommandFailed of string

type file_match_result =
| Matched of file_handler
| Unresolved
| Fatal of string

let sprintf = Printf.sprintf 
let printf = Printf.printf
module Shell = struct
    (** [has_cmd cmd] uses the unix builtin "command" to check if the command [cmd] is available on the system. *)
    let has_cmd cmd = match Unix.system (sprintf "command -v %s &> /dev/null" cmd) with | WEXITED 0 -> true | _ -> false 

    let run cmd args : (int, string) result = 
        let error fmt code = Error(sprintf fmt cmd (string_of_int code)) in
        match Filename.quote_command cmd args |> Unix.system with
        | WEXITED 0 -> Ok(0)
        | WEXITED n -> error "Command `%s` failed \n program aborted with exit code: `%s`." n
        | WSIGNALED n -> error "Command `%s` failed \n program aborted by signal: `%s`." n
        | WSTOPPED n -> error "Command `%s` failed \n stopped by signal: `%s`." n
    
    let print x = print_string x; Ok(0)

    (** [read_process_lines command] runs the [command] using #!/bin/sh until termination
        returning its standard output as a string list where each new line represents 
        one element of the list. *)
    let run_read cmd args = 
        let lines = ref [] in
        let in_channel = Unix.open_process_in (Filename.quote_command cmd args) in
        begin
            try
                while true do lines := input_line in_channel :: !lines done;
            with End_of_file -> ignore (Unix.close_process_in in_channel)
        end;    
        (List.rev !lines)
        |> String.concat "\n"

    (** [print_image] prints the image file [file] in the terminal with the size and position given by [position]
        using a compatible terminal or terminal overlay if possible. *)
    let print_image filename pos =
        let env name = match Sys.getenv_opt name with | Some n -> n | None -> "" in 
        if env "TERM" = "xterm-kitty" then
            match run "kitty" ["+icat"; "--silent"; "--transfer-mode"; "file"; "--place"; (sprintf "%sx%s@%sx%s" pos.width pos.height pos.left pos.top); filename ] with 
            | Error _ -> Error(sprintf "Couldn't print image file `%s` to terminal" filename)
            | _ -> Ok(1)
        else if env "UEBERZUG" = "blub" then Ok(1)
        else Error("")
    let run_print cmd args : (int, string) result = print (run_read cmd args)

    let get_mime file = 
        let full_name = run_read "file" ["--mime-type"; "-b"; file] in
        let length = String.length full_name in
        let seperator_pos = String.index_from full_name 0 '/' in
        let primary = String.sub full_name 0 seperator_pos in
        let param_seperator_pos = 
            match String.index_from_opt full_name seperator_pos ';' with 
            | Some x -> x 
            | None -> length
        in
        let secondary = String.sub full_name (seperator_pos + 1) (param_seperator_pos - seperator_pos - 1) in
        { main = primary; sub = secondary; parameter = None }  
end
module Cache = struct
    let location = match Sys.file_exists "/dev/shm" with | true -> "/dev/shm/"| false -> "/tmp/"
    
    (** [for_file] returns the appropriate unique cache path for a [file] by hashing the file's name.*)
    let for_file file = location ^ (Digest.string file |> Digest.to_hex)
    let exists file = for_file file |> Sys.file_exists 
    let setup = 
      if location |> Sys.is_directory
      then
          match Unix.system ("mkdir -p "  ^ (Filename.quote location)) with
          | WEXITED 0 ->  ()
          | _ -> raise (CommandFailed ("ERROR: 'Cache.setup()' failed could not create cache directory: " ^ (Filename.quote location)) )
end
module Handlers = struct
    let highlight_code file pos =  Shell.run_print "highlight" [ "-W"; "-J"; pos.width; "-O"; "ansi"; file.path ]
    let do_nothing (file) (_ : position) = Shell.print ("Not implemented for file" ^ (Filename.quote file.path))
    let xdg_open file _ = Shell.run "xdg_open" [file.path]
    let print_image file pos = Shell.print_image file.path pos
end
let if_ext ext action = { condition = Extension ext; action = action } 
let if_mime mime_matcher action = { condition = Mime mime_matcher; action = action} 
let rules_preview = 
    [
    if_ext
        [ ".jpg"; ".jpeg"; ".png"; ".ico"; ".bmp"; ".gif"; ".tif"; ".tiff"; ".xpm"; ".apng" ]
        Handlers.print_image;

    if_ext
        [".xml"; ".sql"; ".ini"; ".csv"; ".cs"; ".yaml"; ".yml"; ".py"; ".rb"; ".lua"; 
            ".htm"; ".css"; ".html"; ".php"; ".ml"; ".ps"; ".ps1"; ".java"; ".cpp"; ".c"; ".h"; ".ahk"; ".svg";
            ".tcl"; ".sh"; ".zsh"; ".pl"; ".fs"; ".fsx"; ".js"; ".xsl"; ".sgm"; ".csproj"; ".ent"; ".sgm"; ".vb";
            ".vbs"; ".txt"; ".tsql"; ".text"; ".sml"; ".bash"; ".rs"; ".qml"; ".mssql"; ".md"; ".go"; ".bat"; "fstab"; 
            ".zshrc"; ".bashrc"; ".profile"; "profile"; "xprofile"; ".xinitrc"; ".json"; ".desktop"]
        Handlers.highlight_code;

    if_ext
        [ ".aif"; ".cda"; ".mid"; ".midi"; ".mp3"; ".mpa"; ".ogg"; ".wav"; ".wma"; ".wpl"; ".wmv" ]
        (fun file _ -> Shell.run_print "exiftool" [file.path]);

    if_ext
        [ ".mp4"; ".webm"; ".mkv"; ".avi"; ".mpeg"; ".vob"; ".fl"; ".m2v"; ".mov"; ".m4w"; ".divx" ]
        (fun file position -> 
            let cache_file = Cache.for_file file.path in
            if Sys.file_exists cache_file then
                Shell.print_image cache_file position
            else 
                match Shell.run "ffmpegthumbnailer" ["-i"; file.path; "-o"; cache_file; "-s"; "1024"] with
                | Ok _ -> Shell.print_image cache_file position
                | _ ->  Error(sprintf "ERROR: couldn't create thumbnail from video: " ^ (Filename.quote file.path))
        );

    if_ext (* windows binary files *)
        [ ".exe"; ".msi"; ".dll";] 
        (fun _ _ -> Shell.print "Windows binary file");

    (* document files *)
    if_ext [ ".rtf" ] (fun file _ -> Shell.run_print "textutil" ["-stdout"; "-convert";  "txt"; file.path]);
    if_ext [ ".docx" ] (fun file _ -> Shell.run_print "docx2txt" [file.path; "-"]); 
    if_ext [".doc"] (fun file _ -> Shell.run_print "catdoc" [file.path]);
    if_ext [".odt"; ".ods"; ".odp"; ".sxw"] (fun file _ -> Shell.run_print "odt2txt" [file.path]);
    if_ext [".pdf"] 
        (fun file position -> 
            let cache_file = Cache.for_file file.path in
            if Sys.file_exists (cache_file) then
                Shell.print_image (cache_file ^ ".jpg") position
            else            
                match Shell.run "pdftoppm" ["-jpeg"; "-f"; "1"; "-singlefile"; file.path; cache_file] with
                | Ok _ -> Shell.print_image (cache_file ^ ".jpg") position
                | Error old ->  Error(old ^ Printf.sprintf "ERROR: couldn't create thumbnail from pdf: \"%s\"" file.path)
        );

    (* archive files *)
    if_ext [".iso"] (fun file _ -> Shell.run_print "iso-info" ["--no-header"; "-l"; file.path]);
    if_ext [ ".txz"; ] (fun file _ -> Shell.run_print "tar" ["tjf"; file.path]); 
    if_ext [ ".tar"; ] (fun file _ -> Shell.run_print "tar" ["tf"; file.path]); 
    if_ext [ ".tgz"; ".gz" ] (fun file _ -> Shell.run_print "tar" ["tzf"; file.path]); 
    if_ext [ ".zip"; ".jar"; ".war"; ".ear"; ".oxt"; ] (fun file _ -> Shell.run_print "unzip" ["-l"; file.path]);

    if_mime 
        (fun {sub = sub; _ } -> sub = "empty" || sub = "x-empty") 
        (fun _ _ -> Shell.print "EMPTY FILE");
    
    if_mime 
        (fun { main = main; _ } -> main = "text")
        Handlers.highlight_code
]

let rules_open = [
    (* catch all *)
    if_mime  (fun _ ->  true) Handlers.xdg_open
]

let validate_args file pos result =
    let p = Printf.sprintf in
    let is_int x = match int_of_string_opt x with | None -> false | _ -> true in
    let check check err_msg err_stack = 
        match err_stack with 
        | None -> if check then None else Some(err_msg)
        | Some x -> if check then err_stack else Some(x ^ "\n" ^ err_msg)
    in
    match
        None 
        |> check (Sys.file_exists file) (p "ERROR: the file \"%s\" does not exist" file)
        |> check (is_int pos.width) (p "ERROR: second argument: \"%s\" (width) is not a number." pos.width)
        |> check (is_int pos.height) (p "ERROR: third argument \"%s\" (height) is not a number." pos.height)
        |> check (is_int pos.left) (p "ERROR: fourth argument \"%s\" (x-position/left) is not a number." pos.left)
        |> check (is_int pos.left) (p "ERROR: fifth argument: \"%s\" (y-position/top) is not a number." pos.top)
    with
    | None -> Unresolved
    | Some err -> match result with | Fatal old_err -> Fatal(old_err ^ "\n" ^ err) | _ -> Fatal(err)

let rec evaluate_rules rules file = 
    let evaluate_rule rule file = 
        let file_ext = match Filename.extension file.path with | "" -> file.path | v -> v in
        match rule.condition with
        | Extension extensions -> 
            begin
                match List.find_opt (fun e -> file_ext = e) extensions with
                | None -> Unresolved
                | _ -> Matched(rule.action)
            end
        | Mime comparator -> if file.mime |> comparator then Matched(rule.action) else Unresolved
    in
    match rules with
    | [] -> Unresolved
    | head::tail ->
        begin
            match evaluate_rule head file with
            | Matched result -> Matched(result) 
            | Fatal old -> Fatal ("ERROR: rule could not be matched.\n" ^ old)
            | Unresolved -> evaluate_rules tail file 
        end

let run_handler file print_position rules = 
    match evaluate_rules rules file with
    | Matched handler -> 
        (match handler file print_position with
        | Ok exit_code -> exit_code
        | _ -> 0)
    | Unresolved -> print_string "ERROR:\nNo handler found for this type of file."; 0
    | Fatal m -> print_string ("ERROR:\nAn error has occured while running the file handler:\n " ^ m); 0

let check_dependencies () =
    let cmd_exists cmd = 
        let check_mark = " \027[0;32m✓\027[0;0m" in
        let x_mark = " \027[0;31m✗\027[0;0m" in
        print_endline (cmd ^ if Shell.has_cmd cmd then check_mark else x_mark) in
    let rec check_cmds tools = match tools with | [] -> () | h::t -> cmd_exists h; check_cmds t in
    
    ["highlight"; "kitty"; "ffmpegthumbnailer"; "iso-info" (* libcdio *); "docx2txt"; "odt2txt"; "exiftool"; "catdoc"; "pdftoppm"]
    |> check_cmds 
    |> ignore;
    0

let () =
    let preview file print_position = run_handler file print_position rules_preview in
    let do_open file = run_handler file { width = ""; height = ""; top = ""; left = "" } rules_open in 
    (match Array.length Sys.argv with
    | 3 | 6 ->
        let file_info = { path = Sys.argv.(1); mime = Shell.get_mime Sys.argv.(1) } in
        if Array.length Sys.argv = 6 
        then preview file_info { width = Sys.argv.(2); height = Sys.argv.(3); left = Sys.argv.(4); top = Sys.argv.(5) }
        else do_open file_info
    | 2 ->
        (match Unix.system "kitty +icat --clear --silent --transfer-mode file" with
        | WEXITED code | WSIGNALED code | WSTOPPED code-> code)
    | 1 -> check_dependencies ()
    | _ -> print_string "ERROR: Invalid number of arguments"; 0)
    |> exit