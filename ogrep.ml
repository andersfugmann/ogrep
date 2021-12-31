open Base
open Stdio

let debug = false
let log fmt =
  let module_name =
    match String.substr_index_all ~may_overlap:true Stdlib.__MODULE__ ~pattern:"__" |> List.last with
    | Some index ->
      let pos = index + 2 in
      let len = String.length Stdlib.__MODULE__ - pos in
      String.sub ~pos ~len Stdlib.__MODULE__
    | None -> Stdlib.__MODULE__
  in
  match debug with
  | false -> Printf.ifprintf Out_channel.stderr fmt
  | true -> Stdlib.Printf.printf Stdlib.("%s: " ^^ fmt ^^ "\n") module_name

let _ =
  let dir = Gitignore.remove_trailing_slash (Sys.get_argv ()).(1) in
  let f ~dir ~file ~size = Stdlib.Printf.printf "%8d %s%s\n" size dir file in
  (* First list all files *)
  List_files.list_files ~f dir
