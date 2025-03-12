let usage_msg = "Usage: ./instagram_ratio --following <file1.txt> --by <file2.txt>"

(* Store command line arguments *)
let following_file = ref None
let by_file = ref None

(* Save to the appropriate reference *)
let set_following filename = following_file := Some filename
let set_by filename = by_file := Some filename

(* Argument specifications *)
let speclist = [
  ("--following", Arg.String set_following, "Specify the first file");
  ("-f", Arg.String set_following, "Same as --following");
  ("--by", Arg.String set_by, "Specify the second file");
  ("-b", Arg.String set_by, "Same as --by");
]


(* Read a file into a list of usernames and display names match input_line ic with *)
let read_file filename =
  try
    let ic = open_in filename in
    let rec read_lines acc =
      match input_line ic with
      | line -> read_lines (line :: acc)
      | exception End_of_file ->
          close_in ic;
          List.rev acc
    in
    Some (read_lines [])
  with
  | Sys_error _ -> None

let handle_file filename =
  match read_file filename with
  | Some lines -> lines
  | None ->
      prerr_endline ("Error: Could not read file: " ^ filename);
      prerr_endline usage_msg;
      exit 1

let batch_list list batch_size =
  let rec take n list =
    match (n, list) with
    | (0, _) | (_, []) -> []
    | (n, hd :: tl) -> hd :: take (n - 1) tl
  in
  let rec drop n list =
    match (n, list) with
    | (0, _) | (_, []) -> list
    | (n, _ :: tl) -> drop (n - 1) tl
  in
  let rec batch_list' acc list batch_size =
    match list with
    | [] -> List.rev acc
    | _ ->
        let batch = take batch_size list in
        let rest = drop batch_size list in
        batch_list' (batch :: acc) rest batch_size
  in
  batch_list' [] list batch_size

let not_username str =
  let is_invalid_char c =
    not (
      (c >= 'a' && c <= 'z') ||
      (c >= '0' && c <= '9') ||
      (c = '.') ||
      (c = '_')
    )
  in
  String.exists is_invalid_char str

let check_invalid_batch list =
  let odd = List.exists (fun x -> List.length x <> 2) list in
  let bad_username = List.find_opt (fun x -> not_username (List.hd x)) list in
  match odd, bad_username with
  | _, Some x ->
      prerr_endline ("Error: Badly formatted file. (Likely contains users without display names.) Offending line is: " ^ (List.hd x));
      prerr_endline usage_msg;
      exit 1
  | true, _ ->
      prerr_endline "Error: Badly formatted file. (Likely contains users without display names.)";
      prerr_endline usage_msg;
      exit 1
  | _ -> ()


let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;

  (* Ensure both required arguments are provided *)
  match !following_file, !by_file with
  | Some f, Some b ->
      let following_raw = handle_file f in
      let by_raw = handle_file b in
      let following = batch_list following_raw 2 in
      let by = batch_list by_raw 2 in
      let following_str = List.map (fun x -> String.concat ", " x) following in
      let by_str = List.map (fun x -> String.concat ", " x) by in
      check_invalid_batch following;
      check_invalid_batch by;
      Printf.printf "Following: %s\n" (String.concat "\n" following_str);
      Printf.printf "By: %s\n" (String.concat "\n" by_str);
  | _ ->
      prerr_endline "Error: Both --following (-f) and --by (-b) are required.";
      prerr_endline usage_msg;
      exit 1