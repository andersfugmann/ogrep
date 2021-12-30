open Base
open Stdio

let debug = false
let log fmt = match debug with
  | false -> Printf.ifprintf Out_channel.stderr fmt
  | true -> Stdlib.Printf.printf Stdlib.(fmt ^^ "\n")

let has_traling_slash path = Char.(path.[String.length path - 1] = '/')
let remove_trailing_slash path =
  match has_traling_slash path with
  | true -> String.sub path ~pos:0 ~len:(String.length path - 1)
  | false -> path

(* What if dir is absolute *)
let ls_files dir =
  log "ls_files '%s'" dir;
  let dir_handle =
    match Unix.opendir dir with
    | handle -> handle
    | exception e ->
      log "Cannot open dir '%s' in pwd '%s'" dir (Unix.getcwd ());
      raise e
  in
  let rec inner has_gitignore has_git paths =
    match Unix.readdir dir_handle with
    | path -> begin
        match path, Unix.lstat (dir ^ "/" ^ path) with
        | ".", _ | "..", _ -> inner has_gitignore has_git paths
        | ".git", Unix.{ st_kind = S_DIR; _ } ->
          inner has_gitignore true paths
        | ".gitignore", Unix.{ st_kind = S_REG; st_size; _ } ->
          inner true has_git ((path, st_size) :: paths)
        | _, Unix.{ st_kind = S_REG; st_size; _ } ->
          inner has_gitignore has_git ((path, st_size) :: paths)
        | _, Unix.{ st_kind = S_DIR; _ } ->
          inner has_gitignore has_git ((path ^ "/", -1) :: paths)
        | _, _ -> inner has_gitignore has_git paths
        | exception _ ->
          inner has_gitignore has_git paths
      end
    | exception End_of_file ->
      Unix.closedir dir_handle;
      (has_gitignore, has_git, paths)
  in
  inner false false []

type pattern_type = Include | Exclude

let parse_gitignore_line ~prefix line =
  let line = String.rstrip line in
  let pattern_type, line =
    match String.chop_prefix ~prefix:"!" line with
    | None -> Exclude, line
    | Some line -> Include, line
  in
  let is_absolute =
    String.length line > 0 && String.contains ~len:(String.length line - 1) line '/'
  in
  let prefix = match is_absolute with
    | true when Char.(line.[0] = '/') -> remove_trailing_slash prefix
    | true -> prefix
    | false -> "**/"
  in

  match line with
  | line when String.length line = 0 -> None
  | line when Char.(line.[0] = '#') -> None
  | line ->
    let line = prefix ^ line in
    let line = match has_traling_slash line with
      | false -> line ^ "{,/}"
      | true -> line
    in
    let re = Re.Glob.glob ~expand_braces:true ~anchored:true line |> Re.compile in
    (* Notice that we can merge sequences of pattern *)
    log "Pattern in %s: %s" prefix line;
    Some (pattern_type, re)

(* Need some extra data like dir *)
let parse_gitignore ~cwd dir =
  log "Parse %s%s/.gitignore" cwd dir;
  Stdio.In_channel.read_lines (cwd ^ dir ^ "/.gitignore")
  |> List.filter_map ~f:(parse_gitignore_line ~prefix:dir)

let filter_files filters files =
  let filter file =
    List.fold_left ~init:true ~f:(fun acc (tpe, re) ->
      match tpe, Re.execp re file with
      | Include, true -> true
      | Exclude, true -> false
      | _, false -> acc
    ) filters
  in
  List.filter ~f:(fun (file, _size) -> filter file) files

let id v = v

let rec list_files ~f ~filter ~cwd dir =
  log "list_files ~filter ~cwd:%s ~dir:%s" cwd dir;

  let process_files ~filter ~cwd files =
    files
    |> filter
    |> List.iter ~f:(function
      | file, -1 -> (* Directory *)
        list_files ~f ~filter ~cwd file
      | file, size -> f (cwd ^ file, size)
    )
  in
  let (has_gitignore, has_git, files) = ls_files (cwd ^ dir) in
  match has_gitignore, has_git with
  | true, true ->
    let dir = String.rstrip ~drop:(function '/' -> true | _ -> false) dir in
    let cwd = cwd ^ dir in
    let gitignore = parse_gitignore ~cwd "/" in
    let filter = filter_files gitignore in
    let files = List.map ~f:(fun (file, size) -> "/" ^ file, size) files in
    process_files ~filter ~cwd:(cwd) files
  | false, true ->
    let dir = String.rstrip ~drop:(function '/' -> true | _ -> false) dir in
    let files = List.map ~f:(fun (file, size) -> "/" ^ file, size) files in
    process_files ~filter:id ~cwd:(cwd ^ dir) files
  | true, false ->
    let gitignore = parse_gitignore ~cwd dir in
    let filter files = filter files |> filter_files gitignore in
    let files = List.map ~f:(fun (file, size) -> dir ^ file, size) files in
    process_files ~cwd ~filter files
  | false, false ->
    let files = List.map ~f:(fun (file, size) -> dir ^ file, size) files in
    process_files ~cwd ~filter files

let _ =
  let filter f = f in
  let dir = remove_trailing_slash (Sys.get_argv ()).(1) in
  let f (file, size) = Stdlib.Printf.printf "%8d %s\n" size file in
  list_files ~f ~filter ~cwd:dir "/"
