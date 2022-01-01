open Base
open Stdio

type pattern_type = Include | Exclude

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

let has_traling_slash path = Char.(path.[String.length path - 1] = '/')
let remove_trailing_slash path =
  match has_traling_slash path with
  | true -> String.sub path ~pos:0 ~len:(String.length path - 1)
  | false -> path

let parse_line ~prefix line =
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
    (*let re = Re.Glob.glob ~expand_braces:true ~anchored:true line |> Re.compile in *)
    log "Pattern in %s: %s" prefix line;
    Some (pattern_type, line)

(* Need some extra data like dir *)
let parse ~cwd path file =
  log "Parse %s%s/%s" cwd path file;
  let rec inner (tpe, curr) = function
    | [] when List.is_empty curr -> []
    | [] -> [(tpe, curr)]
    | line :: lines ->
      match parse_line ~prefix:path line with
      | None -> inner (tpe, curr) lines
      | Some (tpe', re) when Poly.(tpe' = tpe) ->
        inner (tpe, re :: curr) lines
      | Some (tpe', re) when List.is_empty curr ->
        inner (tpe', [re]) lines
      | Some (tpe', re) ->
        (tpe', curr) :: inner (tpe', [re]) lines
  in
  Stdio.In_channel.read_lines (cwd ^ path ^ "/" ^ file)
  |> inner (Exclude, [])
  |> List.map ~f:(fun (tpe, res) ->
    let re =
      String.concat ~sep:"," res |> Printf.sprintf "{%s}"
      |> Re.Glob.glob ~anchored:true ~expand_braces:true
      |> Re.compile
    in
    (tpe, re)
  )

let append filters = function
  | [] -> filters
  | filter -> [filter] @ filters

let is_excluded filters file =
  let is_excluded filter file =
    List.fold_left ~init:false ~f:(fun acc (tpe, re) ->
      match tpe, acc with
      | Exclude, false -> Re.execp re file
      | Include, true -> Re.execp re file |> not
      | _ -> acc
    ) filter
  in
  List.exists ~f:(fun filter -> is_excluded filter file) filters

let empty = []

let filter_files filters ~f files =
  List.filter ~f:(fun file -> not (is_excluded filters (f file))) files
