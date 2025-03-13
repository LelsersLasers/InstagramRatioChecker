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

type marked_user = {
  u: string;
  marked: bool;
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

let parse_users lst =
  let mark_display_names lst =
    List.map (fun u -> {u; marked = not_username u}) lst
  in
  let batch_by_marked lst =
    let rec batch_by_marked' acc lst =
      match lst with
      | [] -> List.rev acc
      | {u; marked} :: tl ->
          match marked with
          | true ->
            let new_acc = match acc with
              | [] ->
                prerr_endline ("Error: Badly formatted file. Starts with a display name and not username.");
                prerr_endline usage_msg;
                exit 1
              | hd :: tl' ->
                  let new_hd = {u; marked} :: hd in
                  [] :: (List.rev new_hd) :: tl'
            in
            batch_by_marked' new_acc tl
          | false ->
            match acc with
            | [] -> exit 1 (* Unreachable? *)
            | hd :: tl' ->
                let new_hd = {u; marked} :: hd in
                batch_by_marked' (new_hd :: tl') tl
    in
    batch_by_marked' [[]] lst
  in
  let generate_users lst =
    let rec generate_users' acc lst =
      match lst with
        | [] -> acc
        | hd :: tl ->
            let len = List.length hd in
            let new_eles = match len with
              | 1 -> 
                prerr_endline ("Error: Badly formatted file. Two display names without a username. Offending line is: " ^ (List.hd hd).u);
                prerr_endline usage_msg;
                exit 1
              | n when n mod 2 = 0 ->
                let rec generate_users'' acc' lst' =
                  match lst' with
                  | [] -> List.rev acc'
                  | u1 :: u2 :: tl' ->
                      let new_acc' = {username = u2.u; display_name = u1.u} :: acc' in
                      generate_users'' new_acc' tl'
                  | _ -> exit 1 (* Unreachable *)
                in
                generate_users'' [] hd
              | _ ->
                prerr_endline ("Error: Badly formatted file. Display name without a username. Offending line is: " ^ (List.hd hd).u);
                prerr_endline usage_msg;
                exit 1
            in
            generate_users' (acc @ new_eles) tl
    in
    generate_users' [] lst
  in
  lst |> mark_display_names |> batch_by_marked |> generate_users

(* let check_valid_users list =
  let bad_username = List.find_opt (fun x -> not_username (x.username)) list in
  match bad_username with
  | Some x ->
      prerr_endline ("Error: Badly formatted file. (Likely contains users without display names.) Offending line is: " ^ (x.username));
      prerr_endline usage_msg;
      exit 1
  | _ -> () *)


let list_sub a b =
  List.filter (fun x -> not (List.find_opt (fun y -> x.username = y.username) b |> Option.is_some)) a



let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;

  match !following_file, !by_file with
  | Some f, Some b ->
      let following_raw = handle_file f in
      let by_raw = handle_file b in

      Printf.printf "HERE\n%!";

      let following = parse_users following_raw in
      let by = parse_users by_raw in

      let following_str = List.map user_to_str following in
      let by_str = List.map user_to_str by in
      Printf.printf "Following: %s\n%!" (String.concat "\n" following_str);
      Printf.printf "\nBy: %s\n" (String.concat "\n" by_str);
      
      (* check_valid_users following;
      check_valid_users by; *)

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