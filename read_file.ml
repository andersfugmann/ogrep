open !Eio_linux
open Eio.Std

let find_newline ~offset ~len bigstring =
  Bigstringaf.memchr bigstring offset '\n' len

let rec process_lines ~read ~f bigstring ~src_off buffer ~dst_off =
  (* traceln "process_lines ~src_off:%d ~dst_off:%d" src_off dst_off; *)
  let len = Bigstringaf.length bigstring - src_off in
  match find_newline ~offset:src_off ~len bigstring with
  | -1 -> begin
    Bigstringaf.blit_to_bytes bigstring ~src_off buffer ~dst_off ~len;
    (* This one may raise, but we don't know how yet. *)
    match read () with
    | bigstring ->
      (* traceln "Read %d" (Bigstringaf.length bigstring); *)
      process_lines ~read ~f bigstring ~src_off:0 buffer ~dst_off:(dst_off + len)
    | exception End_of_file ->
      match dst_off > 0 with
      | true -> f ~len:dst_off buffer
      | false -> ()
  end
  | index ->
    let len = index - src_off in
    Bigstringaf.blit_to_bytes bigstring ~src_off buffer ~dst_off ~len;
    (* Ok. Now run g *)
    f ~len:(dst_off + len) buffer;
    process_lines ~read ~f bigstring ~src_off:(index+1) buffer ~dst_off:0

let count = ref 0


let grep_file regex fd =
  let chunk = Eio_linux.alloc () in
  let length = Uring.Region.length chunk in
  let buffer = Bytes.create (max (length * 2) (4096 * 2)) in
  let read () =
    let data_read = Eio_linux.read_upto fd chunk length in
    Uring.Region.to_bigstring ~len:data_read chunk
  in
  let f ~len buffer =
    match Re.execp ~pos:0 ~len regex (Bytes.unsafe_to_string buffer) with
    | true -> traceln "%s" (Bytes.sub_string buffer 0 len)
    | false -> ()
  in
  let chunk = read () in

  match Bigstringaf.memchr chunk 0 '\000' (1000) with
  | -1 -> process_lines ~read ~f chunk ~src_off:0 buffer ~dst_off:0
  | _ -> traceln "Binary file. Skipping"

let read_file _env =
  traceln "Read file. Pattern: '%s'. File: '%s'" Sys.argv.(1) Sys.argv.(2);
  let regex = Re.Pcre.re Sys.argv.(1) |> Re.compile in
  Switch.run @@ fun sw ->
  let fd = Eio_linux.openat2 ~sw ~access:`R ~flags:Uring.Open_flags.noatime ~perm:0
      ~resolve:Uring.Resolve.(empty + no_symlinks) Sys.argv.(2)
  in
  grep_file regex fd

let () = Eio_linux.run read_file
