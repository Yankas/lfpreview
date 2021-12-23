(*
* This program provides file previews for the lf file manager.

* Args
[0] = file name
[1] = available preview width in cells
[2] = available preview height in cells
[3] = horizontal position of the preview window
[4] = vertical position of the preview window

* Output
Preview in text format that represents the preview of the input file ARGS[0]

* Dependency packages:
highlight - syntax highlighting for text files 
kitty - terminal with image support
ffmpegthumbnailer - preview for video files
libcdio - for isoinfo (preview of iso files)
docx2txt - for MS Word docx Preview
odt2txt - for OpenDocument (ODT) preview
perl-image-exiftool - Command exiftool for audio metadata preview
catdoc - Preview for .doc files 

* LF documentation
https://pkg.go.dev/github.com/gokcehan/lf#hdr-Configuration
*)


type position = {  width : string; height: string; left: string; top: string }
type file_type = { 
    extensions : string list;
    handler : string -> position -> (string, string) result
}
type file_match_result =
| Matched of file_type
| Unresolved
| Fatal of string

module Shell = struct

    (** [has_cmd cmd] uses the unix builtin "command" to check if the command [cmd] is available on the system. *)
    let has_cmd cmd = match Unix.system ("command " ^ cmd) with | WEXITED 0 -> true | _ -> false 

    let run cmd : (string, string) result = 
        let error fmt code = Error(Printf.sprintf fmt cmd (string_of_int code)) in
        match Unix.system cmd with
        | WEXITED 0 -> Ok("")
        | WEXITED n -> error "Command `%s` failed \n program aborted with exit code: `%s`." n
        | WSIGNALED n -> error "Command `%s` failed \n program aborted by signal: `%s`." n
        | WSTOPPED n -> error "Command `%s` failed \n stopped by signal: `%s`." n
    
    (** [read_process_lines command] runs the [command] using #!/bin/sh until termination
        returning its standard output as a string list where each new line represents 
        one element of the list. *)
    let run_read_lines prog : (string list, string) result  =
        let lines = ref [] in
            let in_channel = Unix.open_process_in prog in
        begin
            try
                while true do lines := input_line in_channel :: !lines done;
            with End_of_file -> ignore (Unix.close_process_in in_channel)
        end;    
        
        Ok(List.rev !lines)
         
    let run_read prog = 
        match run_read_lines prog with 
        | Ok str -> Ok(str |> String.concat "\n") 
        | e -> Error("TODO: run_read prog failed")

    (** [run_cmd fmt] runs the command specified by [fmt] using the same syntax as Prinft.sprintf
        example: run_cmd "ls %s %s" "-las" "/usr/bin" *)    
    let run_cmd fmt = 
        let do_run str =  Unix.system str |> ignore in 
        Printf.ksprintf do_run fmt

    (** [print_image] prints the image file [file] in the terminal with the size and position given by [position]
        using a compatible terminal or terminal overlay if possible. *)
    let print_image filename pos = 
        match Sys.getenv_opt "TERM" with
        | Some "xterm-kitty" ->
            begin
                match run (Printf.sprintf "kitty +kitten icat --place %sx%s@%sx%s %s" pos.width pos.height pos.left pos.top filename) with
                | Error _ -> Error(Printf.sprintf "Couldn't print image file `%s` to terminal" filename)
                | s ->  Ok("")
            end
        | None | Some _ -> Error("Couldn't display image $TERM is not set")
         
    let run_print cmd = 
        match run_read cmd with
        | Ok x -> print_string x; Ok("")
        | Error x -> Error(x)
end


let ( => ) extensions handler = {extensions; handler}
let file_types = [
    (* code and text files *)
    [".xml"; ".sql"; ".ini"; ".csv"; ".cs"; ".yaml"; ".yml"; ".py"; ".rb"; ".lua"; 
    ".htm"; ".css"; ".html"; ".php"; ".ml"; ".ps"; ".ps1"; ".java"; ".cpp"; ".c"; ".h"; ".ahk"; ".svg";
    ".tcl"; ".sh"; ".zsh"; ".pl"; ".fs"; ".fsx"; ".js"; ".xsl"; ".sgm"; ".csproj"; ".ent"; ".sgm"; ".vb";
    ".vbs"; ".txt"; ".tsql"; ".text"; ".sml"; ".bash"; ".rs"; ".qml"; ".mssql"; ".md"; ".go"; ".bat"; "fstab"; 
    ".zshrc"; ".bashrc"; ".profile"; "profile"; "xprofile"; ".xinitrc"; ".json"]
    => (fun file _ ->  Shell.run_print ("highlight -O ansi " ^ file));

    (* image files *)
    [ ".jpg"; ".jpeg"; ".png"; ".bmp"; ".gif"; ".tif"; ".tiff"; ".xpm" ] => Shell.print_image;


    (* audio files *)
    [ ".aif"; ".cda"; ".mid"; ".midi"; ".mp3"; ".mpa"; ".ogg"; ".wav"; ".wma"; ".wpl"; ".wmv" ]
    => (fun file _ -> Shell.run_print ("exiftool " ^ file));


    (* video files *)
    [ ".mp4"; ".webm"; ".mkv"; ".avi"; ".mpeg"; ".vob"; ".fl"; ".m2v"; ".mov"; ".m4w"; ".divx" ]
    => (fun file position -> 
            let cache_file = "/dev/shm/" ^ file in
            match (Shell.run (Printf.sprintf "ffmpegthumbnailer -i %s -o %s -s 1024" file cache_file)) with
            | Ok _ -> Shell.print_image cache_file position
            | _ ->  Error(Printf.sprintf "Error: couldn't create thumbnail from video: \"%s\"" file)
        );

    (* windows binary files *)
    [ ".exe"; ".msi"; ".dll";] => (fun _ _ -> Shell.run_print "Windows binary file");

    (* document files *)
    [".docx"] => (fun file _ -> Shell.run_print  (Printf.sprintf "docx2txt %s -" file)); 
    [".doc"] => (fun file _ -> Shell.run_print ("catdoc " ^ file));
    [".odt"; ".ods"; ".odp"; ".sxw"] =>  (fun file _ -> Shell.run_print ( "odt2txt " ^ file));

    (* archive files *)
    [".iso"] => (fun file _ -> Shell.run_print ("iso-info --no-header -l " ^ file));
    [ ".txz"; ] => (fun file _ -> Shell.run_print ("tar tjf " ^ file)); 
    [ ".tar"; ] => (fun file _ -> Shell.run_print ("tar tf " ^ file)); 
    [ ".tgz"; ".gz" ] => (fun file _ -> Shell.run_print ("tar tzf " ^ file)); 
    [ ".zip"; ".jar"; ".war"; ".ear"; ".oxt"; ] => (fun file _ -> Shell.run_print ("unzip -l " ^ file));
    
]


let extension_test filename result = 
    let file_ext = match Filename.extension filename with | "" -> filename | v -> v in
    print_string (filename ^ ":" ^ file_ext);
    let rec run_all_tests all_extension_lists = 
        match all_extension_lists with
        | [] -> Unresolved
        | head::tail -> (match (List.find_opt (fun e -> file_ext = e) (head.extensions)) with
            | None -> run_all_tests tail
            | _ -> Matched(head)) in
    
    file_types
    |> run_all_tests

let file_cmd_test filename result = 
    Shell.run ("file " ^ filename) |> ignore; 
    Unresolved


let ( |>> ) x f = match x with | Unresolved -> f x | _ -> x 

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
        |> check (Sys.file_exists file) (p "File: \"%s\" does not exist" file)
        |> check (is_int pos.width) (p "Second argument: \"%s\" (width) is not a number." pos.width)
        |> check (is_int pos.height) (p "Third argument \"%s\" (height) is not a number." pos.height)
        |> check (is_int pos.left) (p "Fourth argument \"%s\" (x-position/left) is not a number." pos.left)
        |> check (is_int pos.left) (p "Fifth argument: \"%s\" (y-position/top) is not a number." pos.top)
    with
    | None -> Unresolved
    | Some err -> match result with | Fatal old_err -> Fatal(old_err ^ "\n" ^ err) | _ -> Fatal(err)
    

let preview () = 
    let file = Sys.argv.(1) in 
    let print_position = { 
        width = Sys.argv.(2);
        height = Sys.argv.(3);
        left = Sys.argv.(4);
        top = Sys.argv.(5);
    } in

    let result = 
        Unresolved
        |>> validate_args file print_position
        |>> extension_test file
        |>> file_cmd_test file 
    in
    begin
        match result with
        | Matched file_type -> file_type.handler file print_position |> ignore; 0
        | Unresolved -> print_string "No handler found for this file type."; 0
        | Fatal m -> print_string ("An error has occured while creating the preview: \n" ^ m); 1
    end

let clean_up () = 
    match Unix.system "kitty +kitten icat --clear" with
    | WEXITED code 
    | WSIGNALED code 
    | WSTOPPED code-> code


let () =
    begin 
        match Array.length Sys.argv with
        | 6 -> preview ()
        | 2 -> clean_up ()
        | _ -> print_string "Error Invalid number of arguments"; 1
    end |> exit