(* let try_write_file dir path data =
  match
    Eio.Dir.with_open_out ~create:(`Exclusive 0o600) dir path @@ fun flow ->
    Eio.Flow.copy_string data flow
  with
  | () -> Eio.Std.traceln "write %s -> ok" path
  | exception ex -> Eio.Std.traceln "write %S -> %a" path Fmt.exn ex

let f () =
  Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  try_write_file cwd "test_file" "hello world"
*)

open Base
open Stdio
module Seq = Stdlib.Seq
let lines = ref 0
let read_lines file =
  In_channel.with_file file ~f:(
    Stdio.In_channel.iter_lines ~f:(fun _ -> Int.incr lines)
  )

(* Create a long list of regexps *)
let parse_gitignore (path_globs, file_globs) dir =
  Stdio.In_channel.with_file (dir ^ "/.gitignore") ~f:(fun in_channel ->
    Stdio.In_channel.input_lines in_channel)
  |> List.fold_left ~init:(path_globs, file_globs) ~f:(fun (path_globs, file_globs) pattern ->
    match String.strip pattern with
    | "" -> (path_globs, file_globs)
    | p when Char.(p.[0] = '#') -> (path_globs, file_globs)
    | p when Char.(p.[0] = '!') ->
      (path_globs, file_globs)
    | p when String.contains p '/' ->
      (* Standard glob pattern *)
      let pattern = String.strip p ~drop:(function '/' -> true | _ -> false) in
      let re =
        dir ^ "/" ^ pattern
        |> Re.Glob.glob ~anchored:true
        |> Re.compile
      in
      (re :: path_globs, file_globs)
    | p ->
      let re =
        p
        |> Re.Glob.glob ~anchored:true
        |> Re.compile
      in
      (path_globs, re :: file_globs)
  )

(* Return a sequence with all files. *)
let find_all_files dir =
  (* We should not filter here, as we dont yet have the .gitignore file *)
  let files_in_dir dir =
    let dir_handle = Unix.opendir dir in
    let rec inner (gitignore, paths) dir_handle =
      match Unix.readdir dir_handle with
      | ".gitignore" -> inner (true, paths) dir_handle
      | path when Char.(path.[0] = '.') ->
        (* Quickly ignore hidden files *)
        inner (gitignore, paths) dir_handle
      | path -> inner (gitignore, path :: paths) dir_handle
      | exception End_of_file -> (gitignore, paths)
    in
    let (has_gitignore, paths) = inner (false, []) dir_handle in
    Unix.closedir dir_handle;
    (has_gitignore, paths)
  in

  let rec inner = function
    | ([]) -> None
    (* How do we pop? Return a new sequence? Yes! *)
    | ((path, ignore_patterns) :: paths) ->
      (* We can optimize this out, as its already returned by readdir *)
      match Unix.stat path with
      | Unix.{ st_kind = S_DIR; _ } ->
        let (has_gitignore, files) = files_in_dir path in
        let ignore_patterns = match has_gitignore with
          | true ->
            let p = parse_gitignore ignore_patterns path in
            p
          | false -> ignore_patterns
        in
        let paths =
          List.fold_left ~init:paths ~f:(fun acc file ->
            let (path_globs, file_globs) = ignore_patterns in
            let filter = List.exists ~f:(fun pattern ->
              Re.execp pattern file) file_globs
            in
            match filter with
            | true -> acc
            | false ->
              let file = path ^ "/" ^ file in
              let filter = List.exists ~f:(fun pattern ->
                Re.execp pattern file) path_globs
              in
              match filter with
              | true -> acc
              | false -> (file, ignore_patterns) :: acc
          ) files
        in
        inner (paths)
      | Unix.{ st_kind = S_REG; _ } ->
        Some (path, paths)
      | _ -> inner paths
      | exception _ -> inner paths
  in
  Sequence.unfold ~init:[dir] ~f:inner

let _ =
  let files = find_all_files ((Sys.get_argv ()).(1), ([], [])) in
  let file_count = Sequence.fold ~init:0 ~f:(fun acc _ -> 1 + acc) files in
  (* read_lines "ogrep.ml"; *)
  printf "Files: %d\n" file_count;
  printf "Lines: %d\n" !lines;
  ()
