open Base
open Stdio

type directory_listing = {
  has_git_dir: bool;
  has_gitignore: bool;
  files: (string * int) list;
}

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

let ls_files dir =
  log "ls_files '%s'" dir;
  let dir_handle =
    match Unix.opendir dir with
    | handle -> handle
    | exception e ->
      log "Cannot open '%s' in '%s'" dir (Unix.getcwd ());
      raise e
  in
  let rec inner ~has_gitignore ~has_git_dir files =
    match Unix.readdir dir_handle with
    | file -> begin
        match file, Unix.lstat (dir ^ "/" ^ file) with
        | ".", _ | "..", _ -> inner ~has_gitignore ~has_git_dir files
        | ".git", Unix.{ st_kind = S_DIR; _ } ->
          inner ~has_gitignore ~has_git_dir:true files
        | ".gitignore", Unix.{ st_kind = S_REG; st_size; _ } ->
          inner ~has_gitignore:true ~has_git_dir ((file, st_size) :: files)
        | _, Unix.{ st_kind = S_REG; st_size; _ } ->
          inner ~has_gitignore ~has_git_dir ((file, st_size) :: files)
        | _, Unix.{ st_kind = S_DIR; _ } ->
          inner ~has_gitignore ~has_git_dir ((file ^ "/", -1) :: files)
        | _, _ -> inner ~has_gitignore ~has_git_dir files
        | exception _ ->
          log "Permission denied on %s/%s" dir file;
          inner ~has_gitignore ~has_git_dir files
      end
    | exception End_of_file ->
      Unix.closedir dir_handle;
      (has_gitignore, has_git_dir, files)
  in
  let (has_gitignore, has_git_dir, files) = inner ~has_git_dir:false ~has_gitignore:false [] in
  { has_gitignore; has_git_dir; files }

let id v = v

let rec process_files_in_dir ~recurse_git_dir ~cwd ~dir ~filter ~f dir_listing =
  let filter = match dir_listing.has_gitignore with
    | true ->
      let gitignore = Gitignore.parse ~cwd dir ".gitignore" in
      (* This should be optimized. We should examine each file individually, as the lists might be rather long *)
      let filter files = filter files |> Gitignore.create_filter gitignore in
      filter
    | false -> filter
  in
  (* Filter all the files *)
  let files = filter dir_listing.files in
  (* Process all files *)
  List.iter ~f:(function
    | (file, -1) -> begin
        let dir = dir ^ file in
        let path = cwd ^ dir in
        let dir_listing = ls_files path in
        match dir_listing.has_git_dir with
        | true ->
          let dir = Gitignore.remove_trailing_slash path in
          recurse_git_dir ~dir dir_listing
        | false ->
          process_files_in_dir ~recurse_git_dir ~cwd ~dir ~filter ~f dir_listing
      end
    | (file, size) ->
      f ~dir:(cwd ^ dir) ~file ~size
  ) files


let list_files ~recurse_git_dir ~f ?dir_listing dir =
  let filter f = f in
  let dir_listing = match dir_listing with
    | None -> ls_files dir
    | Some dir_listing -> dir_listing
  in
  process_files_in_dir ~recurse_git_dir ~cwd:dir ~dir:"/" ~filter ~f dir_listing
