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
(* Whats the stragegy? *)
(* We have at least one buffer.
   Read into the buffer, and find the next newline
   Then copy the line into an ocaml string.

   Once a line has been found, we schedule a task to grep.
   Repeat for each line until the end is reached.

   At this point we know the start and end of the line.
   There might be more data available.

   We could also just use standard blocking calls to read the data.
   We should test how fast we can read.
 *)


open Base
open Stdio
module Seq = Stdlib.Seq
let lines = ref 0
let read_lines file =
  In_channel.with_file file ~f:(
    Stdio.In_channel.iter_lines ~f:(fun _ -> Int.incr lines)
  )


(* Return a sequence with all files *)
(* We need a state. And use Base.Sequence. *)
let find_all_files ~exclude dir =
  let files_in_dir acc dir =
    let dir_handle = Unix.opendir dir in
    let rec inner acc dir_handle =
      match Unix.readdir dir_handle with
      | path when exclude path -> inner acc dir_handle
      | path -> inner ((dir ^ "/" ^ path) :: acc) dir_handle
      | exception End_of_file -> acc
    in
    let paths = inner acc dir_handle in
    Unix.closedir dir_handle;
    paths
  in

  let rec inner = function
    | [] -> None
    | path :: paths ->
       match Unix.stat path with
       | Unix.{ st_kind = S_DIR; _ } ->
          let paths = files_in_dir paths path in
          inner paths
       | Unix.{ st_kind = S_REG; _ } ->
          Some (path, paths)
       | _ -> inner paths
       | exception _ -> inner paths
  in
  Sequence.unfold ~init:[dir] ~f:inner

let _ =
  let exclude = function
    | "_build" -> true
    | "." -> true
    | ".." -> true
    | path when Char.(path.[0] = '.') -> true
    | _ -> false
  in
  (* Ah. We cannot stat, because we need to know the path *)
  let files = find_all_files ~exclude ".." in
  let file_count = Sequence.fold ~init:0 ~f:(fun acc _ -> 1 + acc) files in
  (* read_lines "ogrep.ml"; *)
  printf "Files: %d\n" file_count;
  printf "Lines: %d\n" !lines;
  ()


(* We read a file fully, and ask to find all patterns *)
(* Can we write a regex with supports that? *)
(* We schedule to domains for doing cpu intensive work. *)
(* Reading the dir is blocking. Do we care? As long as we are faster than the domains *)
(* So we need to have fibers reading files. 2 per domain, so we always have the next ready under the assumption that its slow to scan files. (Its not - but still) *)


(** Hmm. Can I mix domainslib and eio?
    I want to do work-stealing. So I want a global queue of tasks.
    A task is process a dir or scan a file.
    Each task is fullfilled with the result.

    Or should I run everything under eio?
    Eio creates a new domain for each call to domain thingy.

    A task is:
| Directory of string
| File of string
| Data of (string * string)
| Result of (string * string list)


A fiber resolves a task....
So its a promise?
We ex



    Maybe we should send lines to the domains as fibers.
*)
