open Base
open Stdio

type task =
  | Ls_files of string
  | Process_file of string * int

let tasks_queue = Eio.Stream.create

let debug = false
let module_name =
  match String.substr_index_all ~may_overlap:true Stdlib.__MODULE__ ~pattern:"__" |> List.last with
  | Some index ->
    let pos = index + 2 in
    let len = String.length Stdlib.__MODULE__ - pos in
    String.sub ~pos ~len Stdlib.__MODULE__
  | None -> Stdlib.__MODULE__
let log fmt =
  match debug with
  | false -> Printf.ifprintf Out_channel.stderr fmt
  | true -> Stdlib.Printf.printf Stdlib.("%s: " ^^ fmt ^^ "\n") module_name



let _ =
  let dir = Gitignore.remove_trailing_slash (Sys.get_argv ()).(1) in
  let f ~file = Stdlib.Printf.printf "%s\n" file in
  let rec recurse_git_dir ~dir dir_listing =
    List_files.list_files ~recurse_git_dir ~f ~dir_listing dir
  in
  List_files.list_files ~recurse_git_dir ~f dir
