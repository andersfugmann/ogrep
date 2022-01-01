open Base
open Eio.Std

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

let find_newline ~offset ~len bigstring =
  Bigstringaf.memchr bigstring offset '\n' len

let rec process_lines ~f ~read bigstring ~src_off buffer ~dst_off =
  let len = Bigstringaf.length bigstring - src_off in
  match find_newline ~offset:src_off ~len bigstring with
  | -1 -> begin
      let dst_off = match dst_off + len >= Bytes.length buffer with
        | true -> 0
        | false ->
          Bigstringaf.blit_to_bytes bigstring ~src_off buffer ~dst_off ~len;
          dst_off + len
      in
      match read () with
      | bigstring ->
        process_lines ~f ~read bigstring ~src_off:0 buffer ~dst_off
      | exception End_of_file ->
        match dst_off > 0 with
        | true -> f ~len:dst_off (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buffer)
        | false -> ()
    end
  | index ->
    let len = index - src_off in
    let () = match dst_off + len with
      | line_length when line_length < Bytes.length buffer ->
        Bigstringaf.blit_to_bytes bigstring ~src_off buffer ~dst_off ~len;
        f ~len:(dst_off + len) (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buffer)
      | _ -> ()
    in
    process_lines ~read ~f bigstring ~src_off:(index+1) buffer ~dst_off:0

let count = ref 0


let iter_lines ~f file fd =
  let chunk = Eio_linux.alloc () in
  let length = Uring.Region.length chunk in
  let buffer = Bytes.create (max (length * 2) (4096 * 2)) in
  let read () =
    let data_read = Eio_linux.read_upto fd chunk length in
    Uring.Region.to_bigstring ~len:data_read chunk
  in
  let () = match read () with
    | chunk when Bigstringaf.memchr chunk 0 '\000' (min 1000 (Bigstringaf.length chunk)) = -1 ->
      process_lines ~read ~f chunk ~src_off:0 buffer ~dst_off:0
    | _ -> log "%s: Binary file" file
    | exception _ -> log "%s: Empty file" file
  in
  Uring.Region.free chunk

let iter_lines ~f file =
  Switch.run @@ fun sw ->
  let fd = Eio_linux.openat2 ~sw ~access:`R ~flags:Uring.Open_flags.(noatime + direct) ~perm:0
      ~resolve:Uring.Resolve.empty file
  in
  iter_lines ~f:(f ~file) file fd
