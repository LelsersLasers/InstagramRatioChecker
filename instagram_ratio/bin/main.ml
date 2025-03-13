let usage_msg = "Usage: ./instagram_ratio --following <file1.txt> --by <file2.txt>"

let following_file = ref None
let by_file = ref None

let set_following filename = following_file := Some filename
let set_by filename = by_file := Some filename

(* Argument specifications *)
let speclist = [
  ("--following", Arg.String set_following, "Specify the first file");
  ("-f", Arg.String set_following, "Same as --following");
  ("--by", Arg.String set_by, "Specify the second file");
  ("-b", Arg.String set_by, "Same as --by");
]

type user = {
  username : string;
  display_name : string;
}

let user_to_str u = u.display_name ^ " (" ^ u.username ^ ")"


let read_file filename =
  try
    let ic = open_in filename in
    let rec read_lines acc =
      match input_line ic with
      | "·" -> read_lines acc
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

let parse_users lst =
  let rec parse_users' acc lst =
    match lst with
    | [] -> List.rev acc
    | username :: display_name :: tl -> parse_users' ({username; display_name} :: acc) tl
    | username :: [] -> parse_users' ({username; display_name = "!"} :: acc) []
  in
  parse_users' [] lst


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

let check_valid_users list =
  let bad_username = List.find_opt (fun x -> not_username (x.username)) list in
  match bad_username with
  | Some x ->
      prerr_endline ("Error: Badly formatted file. (Likely contains users without display names.) Offending line is: " ^ (x.username));
      prerr_endline usage_msg;
      exit 1
  | _ -> ()


let list_sub a b =
  List.filter (fun x -> not (List.find_opt (fun y -> x.username = y.username) b |> Option.is_some)) a



let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;

  (* Ensure both required arguments are provided *)
  match !following_file, !by_file with
  | Some f, Some b ->
      let following_raw = handle_file f in
      let by_raw = handle_file b in

      (* let following = batch_list following_raw 2 in
      let by = batch_list by_raw 2 in *)

      let following = parse_users following_raw in
      let by = parse_users by_raw in

      (* let following_str = List.map (fun x -> "[" ^ (String.concat ", " x) ^ "]") following in
      let by_str = List.map (fun x -> "[" ^ (String.concat ", " x) ^ "]") by in
      Printf.printf "Following: %s\n" (String.concat "\n" following_str);
      Printf.printf "\nBy: %s\n" (String.concat "\n" by_str); *)
      
      check_valid_users following;
      check_valid_users by;

      let following_not_by = list_sub following by in
      let by_not_following = list_sub by following in
      
      let following_not_by_str = List.map user_to_str following_not_by in
      let by_not_following_str = List.map user_to_str by_not_following in
      
      Printf.printf "Following but not by: %s\n" (String.concat "\n" following_not_by_str);
      Printf.printf "\nBy but not following: %s\n" (String.concat "\n" by_not_following_str)
  | _ ->
      prerr_endline "Error: Both --following (-f) and --by (-b) are required.";
      prerr_endline usage_msg;
      exit 1