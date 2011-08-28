(*
  Line count with estimation of total number of lines.
*)

(*
  TODO: support for files over 1GB on 32-bit platforms
*)

open Printf

let every = 1_000_000

let count_display_width = ref (-1)

let rec select f = function
    [] -> []
  | x :: l ->
      match f x with
          None -> select f l
        | Some y -> y :: select f l

let readable_string_of_int n =
  let rec loop digits n =
    if n < 0 then "-" ^ loop digits (-n)
    else if n = 0 then "0"
    else
      let s = string_of_int (n mod 10) in
      let n = n / 10 in
      if n <> 0 then
        let sep =
          if (digits+1) mod 3 = 0 then ","
          else ""
        in
        loop (digits+1) n ^ sep ^ s
      else s
  in
  loop 0 n

let get_info fname =
  let x = Unix.stat fname in
  if x.Unix.st_kind <> Unix.S_REG then (
    eprintf "Ignoring %S: not a regular file\n%!" fname;
    None
  )
  else
    Some (fname, x)

let get_total_bytes l =
  List.fold_left (fun acc (fname, x) -> acc + x.Unix.st_size) 0 l

let clear_progress () =
  printf "\r\x1B[K%!"

let print_progress total_bytes fname bytes lines =
  let progress = float bytes /. float total_bytes in
  let total_lines = truncate (float lines /. progress) in
  if !count_display_width < 0 then
    count_display_width :=
      String.length (readable_string_of_int (5 * total_lines));
  printf "%3.0f%% [%s] projected line count: %s %!"
    (100. *. progress)
    (Filename.basename fname)
    (readable_string_of_int total_lines)

let find_lines_in_chunk
    total_bytes initial_bytes initial_lines fname buf len =
  let local_lines = ref 0 in
  for i = 0 to len - 1 do
    match String.unsafe_get buf i with
        '\n' ->
          incr local_lines;
          if (initial_lines + !local_lines) mod every = 0 then (
            clear_progress ();
            print_progress
              total_bytes
              fname
              (initial_bytes + i)
              (initial_lines + !local_lines)
          )
      | _ -> ()
  done;
  !local_lines

let refill fd buf =
  let maxlen = String.length buf in
  let len = Unix.read fd buf 0 maxlen in
  assert (len >= 0);
  len

let rec read_file total_bytes initial_bytes initial_lines fname fd buf =
  match refill fd buf with
      0 -> initial_lines
    | chunk_bytes ->
        let chunk_lines =
          find_lines_in_chunk
            total_bytes initial_bytes initial_lines fname buf chunk_bytes
        in
        read_file
          total_bytes
          (initial_bytes + chunk_bytes) (initial_lines + chunk_lines)
          fname fd buf 

let count total_bytes initial_bytes initial_lines fname =
  let fd = Unix.openfile fname [Unix.O_RDONLY] 0 in
  try
    let buf = String.create (1024 * 1024) in
    let lines =
      read_file total_bytes initial_bytes initial_lines fname fd buf in
    Unix.close fd;
    lines
  with e ->
    (try Unix.close fd with _ -> ());
    raise e

let string_of_count n =
  let count_s = readable_string_of_int n in
  if !count_display_width < 0 then
    count_display_width := 10;
  let blank =
    let len = max 0 (!count_display_width - String.length count_s) in
    String.make len ' '
  in
  blank ^ count_s

let main () =
  let options = [] in
  let usage_msg = sprintf "\
Usage: %s [OPTIONS] FILE1 [FILE2 ...]

wcl is a replacement for \"wc -l\" useful for interactive use
with large data files. Like \"wc -l\" it counts the number of lines
in files and displays the total. Unlike \"wc -l\", wcl displays a progress 
meter and an estimate of the total number of lines as it progresses.


Options:
"
    Sys.argv.(0)
  in
  let files = ref [] in
  let anon_fun s =
    files := s :: !files in
  Arg.parse options anon_fun usage_msg;

  let files = List.rev !files in
  let l = select get_info files in
  let total_bytes = get_total_bytes l in
  let _, total_lines =
    List.fold_left (
      fun (bytes0, lines0) (fname, info) ->
        let lines = count total_bytes bytes0 lines0 fname in
        let file_lines = lines - lines0 in
        clear_progress ();
        printf "%s %s\n%!" (string_of_count file_lines) fname;
        (bytes0 + info.Unix.st_size, lines)
    ) (0, 0) l
  in
  printf "%s total\n%!" (string_of_count total_lines)

let () =
  main ()
