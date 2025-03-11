let usage_msg = "Usage: ./instagram_ratio --following <file1.txt> --by <file2.txt>"

(* References to store argument values *)
let following_file = ref None
let by_file = ref None

(* Function to set the file references *)
let set_following filename = following_file := Some filename
let set_by filename = by_file := Some filename

(* Define argument specifications *)
let speclist = [
  ("--following", Arg.String set_following, "Specify the first file");
  ("-f", Arg.String set_following, "Same as --following");
  ("--by", Arg.String set_by, "Specify the second file");
  ("-b", Arg.String set_by, "Same as --by");
]

(* Main execution *)
let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;

  (* Ensure both required arguments are provided *)
  match !following_file, !by_file with
  | Some f, Some b ->
      Printf.printf "Following file: %s\nBy file: %s\n" f b
  | _ ->
      prerr_endline "Error: Both --following (-f) and --by (-b) are required.";
      prerr_endline usage_msg;
      exit 1