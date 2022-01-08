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

type t = {
  chunk: Uring.Region.chunk;
  buffer: Bytes.t;
}

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

let iter_lines t ~f file =
  let length = Uring.Region.length t.chunk in
  Switch.run @@ fun sw ->
  let fd = Eio_linux.openat2 ~sw ~access:`R ~flags:Uring.Open_flags.(noatime + direct) ~perm:0
      ~resolve:Uring.Resolve.empty file
  in
  let read () =
    let data_read = Eio_linux.read_upto fd t.chunk length in
    log "Read: %d" data_read;
    Uring.Region.to_bigstring ~len:data_read t.chunk
  in
  let () = match read () with
    | chunk when Bigstringaf.memchr chunk 0 '\000' (min 1000 (Bigstringaf.length chunk)) = -1 ->
      process_lines ~read ~f chunk ~src_off:0 t.buffer ~dst_off:0
    | _ -> log "%s: Binary file" file
    | exception _ -> log "%s: Empty file" file
  in
  Eio_linux.FD.close fd

let init () =
  let chunk = Eio_linux.alloc () in
  let length = Uring.Region.length chunk in
  let buffer = Bytes.create (max (length * 2) (4096 * 2)) in
  { chunk; buffer }

let deinit { chunk; _ } =
  Eio_linux.free chunk

let read ~dir file =
  Eio.Dir.with_open_in dir file @@ fun source ->
  Eio.Dir.with_open_out ~create:`Never dir "/dev/null" @@ fun sink ->
  Eio.Flow.copy source sink
