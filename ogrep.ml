open Base
open! Stdio

let domains = 4
let fibers_per_domain = 2
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

let process_file_task ~f _env =
  function
  | "" ->  log "Given empty file"
  | file ->
    try
      f file;
      log "Done: %s" file
    with _ ->
      log "Failed on file: %s" file

let task_runner ~f env id queue =
  let grep_t = Read_file.init () in
  let rec inner () =
    let task = Eio.Stream.take queue in
    (* log "Got Task. id: %d. Tasks: %d. %s" id !tasks (string_of_task task); *)
    match task with
    | Dir (dir, dir_listing) ->
      List_files.list_files ~recurse_git_dir:(fun ~dir dir_listing -> post_dir_tasks ~queue ~dir_listing dir) ~f:(post_file_task queue) ?dir_listing dir;
      task_done queue;
      inner ()
    | File file ->
      process_file_task ~f:(f grep_t) env file;
      task_done queue;
      inner ()
    | Done ->
      log "Done";
      Eio.Stream.add queue Done
  in
  log "Starting task runner %d" id;
  inner ();
  Read_file.deinit grep_t

let start_domain ~f ~env id queue =
  let _domain_mgr = Eio.Stdenv.domain_mgr env in
  log "Starting domain %d" id;
  env#domain_mgr#run @@ fun () ->
  log "Domain started";
  List.init fibers_per_domain ~f:(fun _ () -> task_runner ~f env id queue)
  |> Eio.Std.Fibre.all

let start_domains ~f env queue =
  log "Starting domains: %d" domains;
  let fibers = List.init domains ~f:(fun id () -> start_domain ~f ~env id queue) in
  log "Starting fibers";
  Eio.Std.Fibre.all fibers;
  log "Fibers done"


let grep ~env ~re ~file ~len line =
  match Re.execp ~pos:0 ~len re line with
  | true -> Eio.Flow.copy_string (Printf.sprintf "%s: %s\n" file (String.sub line ~pos:0 ~len)) env#stdout
  | false -> ()

let grep_re2 ~env ~re ~file ~len line =
  let line = String.sub line ~pos:0 ~len in
  match Re2.matches re line with
  | true -> Eio.Flow.copy_string (Printf.sprintf "%s: %s\n" file line) env#stdout
  | false -> ()

let grep_ignore ~env ~re ~file ~len line =
  ignore (env, re, file, len, line)

let match_line = true
let grep = match match_line with
  | true -> grep
  | false -> grep_ignore

let _copy_file ~env ~sw ~re =
  ignore re;
  fun source ->
    let sink = env#fs#open_out ~sw ~append:false ~create:`Never "/dev/null" in
    let source = env#fs#open_in ~sw source in
    Eio.Flow.copy source sink;
    sink#close;
    source#close

let copy_file ~env ~sw ~re t file =
  ignore (env, sw);
  Read_file.iter_lines t ~f:(grep ~env ~re ~file)  file

let copy_file_re2 ~env ~sw ~re t file =
  ignore (env, sw);
  Read_file.iter_lines t ~f:(grep_re2 ~env ~re ~file)  file


let _ =
  let argv = Sys.get_argv () in
  let re = Re.Pcre.re argv.(1) |> Re.compile in
  let _re = Re2.create argv.(1) |> Or_error.ok_exn in
  let dir = Gitignore.remove_trailing_slash argv.(2) in
  Eio_linux.run ~queue_depth:1024 ~block_size:65535 (fun env ->
    Eio.Std.Switch.run (fun sw ->
      log "%s %s %s" argv.(0) argv.(1) argv.(2);
      let queue = Eio.Stream.create task_queue_length in
      post_dir_tasks ~queue dir;
      start_domains ~f:(copy_file ~env ~sw ~re) env queue
    )
  );
  Stdlib.Gc.print_stat Out_channel.stderr;
  ()
