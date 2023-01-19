(*


*)

type filetype = {
    extensions: string list;
    executable: bool;
    skeleton: string
}
let file_types = 
    [
        {
            extensions = ["bash"]; 
            executable = true; 
            skeleton = "#!/bin/sh\n\n"
        };
        {
            extensions = ["fsharp"; "fsi"; "fsx"]; 
            executable = true; 
            skeleton = "#!/usr/bin/env dotnet fsi\n\n"
        };
        {
            extensions = ["html"; "htm"]; 
            executable = true; 
            skeleton = "<!DOCTYPE html>\n<html>\n\t<head>\n\t</head>\
                        \t<body>\n\t\t\n\t</body>\n</html>"
        };
        {
            extensions = ["json"]; 
            executable = false;
            skeleton = "{\n\n}"
        };
        {
            extensions = ["lua"]; 
            executable = true; 
            skeleton = "#!/usr/bin/env lua\n\n"
        };
        {
            extensions = ["pearl"; "pl"]; 
            executable = true; 
            skeleton = "#!/usr/bin/env pearl\n\n"
        };
        {
            extensions = ["powershell"; "ps1"; "pwsh"]; 
            executable = true; 
            skeleton = "#!/usr/bin/env pwsh\n\n"
        };
        { 
            extensions = ["python"; "py"]; 
            executable = true;
            skeleton = "#!/usr/bin/env python\n\ndef main():\
            \n\nif __name__ == '__main__':\nmain()"
        };
        { 
            extensions = ["ruby"; "rb"]; 
            executable = true; 
            skeleton = "#!/usr/bin/env ruby\n\n"
        };
        {
            extensions = ["shell"; "sh"]; 
            executable = true; 
            skeleton = "#!/bin/sh\n\n"
        };
        {
            extensions = ["tclsh"; "tcl"]; 
            executable = true; 
            skeleton = "#!/usr/bin/env tclsh\n\n"
        };
        { 
            extensions = ["xml"]; 
            executable = false; 
            skeleton = "<?xml version=\"1.0\" encoding=\"UTF-8\" >?"
        };
        { 
            extensions = ["yaml"; "yml"]; 
            executable = false; 
            skeleton = "---\n"
        };
        {
            extensions = ["zshell"; "zsh"]; 
            executable = true; 
            skeleton = "#!/bin/sh\n\n"
        }
    ]


let usage = "mkscript [-ev][-x|-n] FILE_NAME [FILE_TYPE]"
let pos_args = ref []
let anon_fun positional_args = pos_args := positional_args :: !pos_args
let verbose = ref false
let force_executable = ref false
let force_not_executable = ref false
let opt_list =
    [
        ("-v", Arg.Set verbose, "Output debug information");
        ("-x", Arg.Set force_executable, "Force setting of executable flag");
        ("-n", Arg.Set force_not_executable, "Do not set executable flag");
        ("-e", Arg.Set verbose, "Open the created file using $VISUAL or $EDITOR.");
    ]

let create_file file_name is_executable content =
    let perms = if is_executable then 0o774 else 0o664 in
    let oc = open_out_gen [Open_creat] perms file_name in
    Printf.fprintf oc "%s" content

let cont x y = match y with | Ok value -> x(value) | Error _ -> y

let validate_args () =
    match pos_args with
    | [] -> Error("No file name specified, see mkscript -h for help.")
    | [x] -> Ok(name)


let () =
    Arg.parse opt_list anon_fun usage
    let extension = "test"
    let name = "blub"
    let f_type =
        let rec get_ftype (f_type_list) =
            match f_type_list with
            | [] -> None
            | head::tail -> 
                (match (List.find_opt (fun ext -> ext = extension) (head.extensions)) with
                | Some _ -> Some(head)
                | None -> get_ftype tail)
            in
        get_ftype file_types;;


    match f_type with
    | Some f -> 
        create_file name f.executable f.skeleton
    | None -> 
        failwith "Filetype could not be determined."
    


                 
             
   
