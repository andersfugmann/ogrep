open Base
open! Stdio

let domains = 4
let task_queue_length = 10000000

let debug = false
let formatter = Stdlib.Format.get_err_formatter ()
let module_name =
  match String.substr_index_all ~may_overlap:true Stdlib.__MODULE__ ~pattern:"__" |> List.last with
  | Some index ->
    let pos = index + 2 in
    let len = String.length Stdlib.__MODULE__ - pos in
    String.sub ~pos ~len Stdlib.__MODULE__
  | None -> Stdlib.__MODULE__
let log fmt =
  match debug with
  | false -> Stdlib.Format.ifprintf formatter fmt
  | true -> Eio.Std.traceln Stdlib.("%s: " ^^ fmt) module_name

(** Start some domains to accept tasks *)
type task =
  | Dir of string * List_files.directory_listing option
  | File of string
  | Done

let string_of_task = function
  | Dir (dir, _) -> Printf.sprintf "Dir %s" dir
  | File file -> Printf.sprintf "File %s" file
  | Done -> "Done"

module Mutex = Stdlib.Mutex
let mutex = Mutex.create ()
let tasks = ref 0

let incr_tasks () =
  Mutex.lock mutex;
  Int.incr tasks;
  Mutex.unlock mutex

let decr_tasks_test_zero () =
  Mutex.lock mutex;
  Int.decr tasks;
  let is_zero = (!tasks = 0) in
  Mutex.unlock mutex;
  is_zero

let task_done queue =
  Mutex.lock mutex;
  Int.decr tasks;
  let () = match (!tasks = 0) with
    | true ->
      Int.incr tasks;
      Mutex.unlock mutex;
      Eio.Stream.add queue Done
    | false ->
      Mutex.unlock mutex;
      ()
  in
  ()

let post_dir_tasks ~queue ?dir_listing dir =
  incr_tasks ();
  Eio.Stream.add queue (Dir (dir, dir_listing))

let post_file_task queue ~file =
  incr_tasks ();
  Eio.Stream.add queue (File file)

let process_file_task ~f _env file =
  log "%s" file;
  Read_file.iter_lines ~f:f file

let task_runner ~f env id queue =
  let rec inner () =
    let task = Eio.Stream.take queue in
    log "Got Task. id: %d. Tasks: %d. %s" id !tasks (string_of_task task);
    match task with
    | Dir (dir, dir_listing) ->
      List_files.list_files ~recurse_git_dir:(fun ~dir dir_listing -> post_dir_tasks ~queue ~dir_listing dir) ~f:(post_file_task queue) ?dir_listing dir;
      task_done queue;
      inner ()
    | File file ->
      process_file_task ~f env file;
      task_done queue;
      inner ()
    | Done ->
      task_done queue;
      ()
  in
  log "Starting task runner %d" id;
  inner ()

let start_domain ~f domain_mgr id queue =
  log "Starting domain %d" id;
  Eio.Domain_manager.run domain_mgr
  (fun () ->
    log "Domain started";
    Eio_linux.run (fun env -> task_runner ~f env id queue)
  )

let start_domains ~f env queue =
  log "Starting domains: %d" domains;
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let fibers = List.init domains ~f:(fun id -> fun () -> start_domain ~f domain_mgr id queue) in
  log "Starting fibers";
  Eio.Std.Fibre.all fibers;
  log "Fibers done";
  ()

let grep ~re ~file ~len line =
  match Re.execp ~pos:0 ~len re line with
  | true -> Stdlib.Printf.printf "%s: %s\n" file (String.sub line ~pos:0 ~len)
  | false -> ()

let grep_ignore ~re ~file ~len line =
  ignore (re, file, len, line)

let match_line = true
let grep = match match_line with
  | true -> grep
  | false -> grep_ignore

let _ =
  let argv = Sys.get_argv () in
  let re = Re.Pcre.re argv.(1) |> Re.compile in
  let dir = Gitignore.remove_trailing_slash argv.(2) in

  Eio_linux.run (fun env ->
    log "%s %s %s" argv.(0) argv.(1) argv.(2);
    let queue = Eio.Stream.create task_queue_length in
    post_dir_tasks ~queue dir;
    start_domains ~f:(grep ~re) env queue
  )
