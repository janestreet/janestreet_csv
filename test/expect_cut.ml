open! Core
open! Async
open! Import

let example_csv =
  { Csv_common.header = [ "fruit"; "quantity"; "owner" ]
  ; lines =
      [ [ "apple"; "4"; "Abraham" ]
      ; [ "apple"; "6"; "Bathsheba" ]
      ; [ "orange"; "2"; "Cyrus" ]
      ]
  }
;;

let make_input_csv ?sep filename =
  let csv_lines = example_csv.header :: example_csv.lines in
  make_input_csv ?sep filename csv_lines
;;

(* Cut one column. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "cut"; "-fields"; "fruit"; "input.csv" ] in
    [%expect {|
    fruit
    apple
    apple
    orange |}];
    return ())
;;

(* Cut two columns. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "cut"; "-fields"; "fruit,quantity"; "input.csv" ] in
    [%expect {|
    fruit,quantity
    apple,4
    apple,6
    orange,2 |}];
    return ())
;;

(* Cut all but two columns. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "cut"; "-v"; "-fields"; "fruit,quantity"; "input.csv" ] in
    [%expect {|
    owner
    Abraham
    Bathsheba
    Cyrus |}];
    return ())
;;

(* Cut out owner column. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "cut"; "-v"; "-fields"; "owner"; "input.csv" ] in
    [%expect {|
    fruit,quantity
    apple,4
    apple,6
    orange,2 |}];
    return ())
;;

(* Pass -sh. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "cut"; "-sh"; "-fields"; "fruit"; "input.csv" ] in
    [%expect {|
    apple
    apple
    orange |}];
    return ())
;;

(* Use the wrong delimiter. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      run
        ~enable_ocaml_backtraces:false
        "csv"
        [ "cut"; "-d"; "."; "-fields"; "fruit"; "input.csv" ]
    in
    [%expect
      {|
    ("Unclean exit" (Exit_non_zero 1))
    --- STDERR ---
    Uncaught exception:

      ("Exception raised in Delimited.Read" (line_number 1) (header_map ())
       (buffer (fruit,quantity,owner)) (exn ("Unknown header" fruit))) |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      run
        ~enable_ocaml_backtraces:false
        "csv"
        [ "cut"; "-d"; "."; "-fields"; "fruit,quantity,owner"; "input.csv" ]
    in
    [%expect
      {|
    ("Unclean exit" (Exit_non_zero 1))
    --- STDERR ---
    Uncaught exception:

      ("Exception raised in Delimited.Read" (line_number 1) (header_map ())
       (buffer (fruit,quantity,owner)) (exn ("Unknown header" fruit))) |}];
    return ())
;;

(* Use the correct, unusual delimiter. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () = run "csv" [ "cut"; "-d"; "."; "-fields"; "fruit"; "input.csv" ] in
    [%expect {|
    fruit
    apple
    apple
    orange |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () =
      run "csv" [ "cut"; "-d"; "."; "-fields"; "fruit,quantity"; "input.csv" ]
    in
    [%expect {|
    fruit,quantity
    apple,4
    apple,6
    orange,2 |}];
    return ())
;;

let%expect_test "incorrect duplication of header" =
  (* We used to have a bug where the header would be printed once per file. *)
  do_test (fun () ->
    let%bind () = make_input_csv "1.csv" in
    let%bind () = make_input_csv "2.csv" in
    let%bind () = run "csv" [ "cut"; "-fields"; "quantity"; "1.csv"; "2.csv" ] in
    [%expect {|
      quantity
      4
      6
      2
      4
      6
      2 |}];
    return ())
;;

let%expect_test "properly cut a csv that's loaded into memory" =
  let f row = Csvlib.Csv.print [ Array.to_list row ] in
  let () =
    Cut.cut_by_field_names
      (Csv example_csv)
      ~sep:','
      ~skip_header:false
      ~f
      (`All_but [ "quantity" ])
  in
  return
    [%expect {|
    fruit,owner
    apple,Abraham
    apple,Bathsheba
    orange,Cyrus
|}]
;;
