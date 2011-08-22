(*
  Line count with estimation of total number of lines.
*)

(*
  TODO: support for files over 1GB on 32-bit platforms
  TODO: option for progress frequency
  TODO: option for printing ints without commas
*)

open Printf

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
  let total_lines = float lines /. progress in
  printf "%3.0f%% [%s] projected line count: %s %!"
    (100. *. progress)
    (Filename.basename fname)
    (readable_string_of_int (truncate total_lines))

let count enable_progress every total_bytes initial_bytes initial_lines fname =
  let ic = open_in fname in
  let local_lines = ref 0 in
  try
    while true do
      ignore (input_line ic);
      incr local_lines;
      if enable_progress && !local_lines mod every = 0 then (
        let local_bytes = pos_in ic in 
        clear_progress ();
        print_progress
          total_bytes
          fname (initial_bytes + local_bytes) (initial_lines + !local_lines)
      )
    done;
    assert false
  with
      End_of_file ->
        close_in ic;
        !local_lines
    | e ->
        close_in_noerr ic;
        raise e

let main () =
  let enable_progress = true in
  let every = 1_000_000 in
  let files = List.tl (Array.to_list Sys.argv) in
  let l = select get_info files in
  let total_bytes = get_total_bytes l in
  let _, total_lines =
    List.fold_left (
      fun (bytes, lines) (fname, info) ->
        let local_lines =
          count enable_progress every total_bytes bytes lines fname
        in
        clear_progress ();
        printf "%s %s\n%!" (readable_string_of_int local_lines) fname;
        (bytes + info.Unix.st_size, lines + local_lines)
    ) (0, 0) l
  in
  printf "%s total\n%!" (readable_string_of_int total_lines)

let () =
  main ()
