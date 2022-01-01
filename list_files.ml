open Base
open Stdio

type directory_listing = {
  has_git_dir: bool;
  has_gitignore: bool;
  files: string list;
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
  | true -> Stdlib.Printf.printf Stdlib.("%s: " ^^ fmt ^^ "\n%!") module_name

let ls_files dir =
  let dir = match dir with "" -> "/" | dir -> dir in
  let rec inner dir_handle ~has_gitignore ~has_git_dir files =
    match Dirent_unix.readdir dir_handle with
    | Dirent.Dirent.{ name = "."; kind = Dirent.File_kind.DT_DIR; _ }
    | Dirent.Dirent.{ name = ".."; kind = Dirent.File_kind.DT_DIR; _ } ->
      inner dir_handle ~has_gitignore ~has_git_dir files
    | Dirent.Dirent.{ kind = Dirent.File_kind.DT_DIR; name = ".git"; _ } ->
      inner dir_handle ~has_gitignore ~has_git_dir:true files
    | Dirent.Dirent.{ kind = Dirent.File_kind.DT_DIR; name; _ } ->
      inner dir_handle ~has_gitignore ~has_git_dir ((name ^ "/") :: files)
    | Dirent.Dirent.{ kind = Dirent.File_kind.DT_REG; name = ".gitignore"; _ } ->
      inner dir_handle ~has_gitignore:true ~has_git_dir (".gitignore" :: files)
    | Dirent.Dirent.{ kind = Dirent.File_kind.DT_REG; name; _ } ->
      inner dir_handle ~has_gitignore ~has_git_dir (name :: files)
    | _ -> inner dir_handle ~has_gitignore ~has_git_dir files
    | exception End_of_file ->
      (has_gitignore, has_git_dir, files)
    | exception _ ->
      log "Permission denied on %s" dir;
      inner dir_handle ~has_gitignore ~has_git_dir files
  in
  log "ls_files '%s'" dir;
  match Dirent_unix.opendir dir with
  | dir_handle ->
    let (has_gitignore, has_git_dir, files) =
      inner dir_handle ~has_git_dir:false ~has_gitignore:false []
    in
    Dirent_unix.closedir dir_handle;
    { has_gitignore; has_git_dir; files }
  | exception _ ->
    { has_gitignore = false; has_git_dir = false; files = [] }

let endswith s c =
  Char.(s.[String.length s - 1] = c)

let rec process_files_in_dir ~recurse_git_dir ~cwd ~dir ~filters ~f dir_listing =
  let filters = match dir_listing.has_gitignore with
    | true ->
      let filter = Gitignore.parse ~cwd dir ".gitignore" in
      Gitignore.append filters filter
    | false -> filters
  in
  (* Process all files *)
  List.iter ~f:(function
      f when Char.(f.[0] = '.') -> ()
    | file ->
    let file = dir ^ file in
    match Gitignore.is_excluded filters file with
    | true -> ()
    | false when endswith file '/' -> begin
        let path = cwd ^ file in
        let dir_listing = ls_files path in
        match dir_listing.has_git_dir with
        | true ->
          let dir = Gitignore.remove_trailing_slash path in
          recurse_git_dir ~dir dir_listing
        | false ->
          process_files_in_dir ~recurse_git_dir ~cwd ~dir:file ~filters ~f dir_listing
      end
    | false ->
      f ~file:(cwd ^ file)
  ) dir_listing.files


let list_files ~recurse_git_dir ~f ?dir_listing dir =
  let dir_listing = match dir_listing with
    | None -> ls_files dir
    | Some dir_listing -> dir_listing
  in
  process_files_in_dir ~recurse_git_dir ~cwd:dir ~dir:"/" ~filters:Gitignore.empty ~f dir_listing
