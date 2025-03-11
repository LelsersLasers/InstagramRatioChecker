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


let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;

  (* Ensure both required arguments are provided *)
  match !following_file, !by_file with
  | Some f, Some b ->
      let following = read_file f in
      let by = read_file b in
      (match following, by with
      | Some f, Some b ->
          Printf.printf "Following: %s\n" (String.concat ", " f);
          Printf.printf "By: %s\n" (String.concat ", " b)
      | None, _ ->
          prerr_endline ("Error: Could not read file: " ^ f);
          prerr_endline usage_msg;
          exit 1
      | _, None ->
          prerr_endline ("Error: Could not read file: " ^ b);
          prerr_endline usage_msg;
          exit 1)
  | Some _, None ->
      prerr_endline "Error: --by (-b) argument is required.";
      prerr_endline usage_msg;
      exit 1
  | None, Some _ ->
      prerr_endline "Error: --following (-f) argument is required.";
      prerr_endline usage_msg;
      exit 1
  | None, None ->
      prerr_endline "Error: Both --following (-f) and --by (-b) are required.";
      prerr_endline usage_msg;
      exit 1