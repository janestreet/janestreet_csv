open! Core
open! Async
open Import

let make_input_csv ?sep filename =
  make_input_csv
    ?sep
    filename
    [ [ "fruit"; "quantity"; "owner" ]
    ; [ "apple"; "4"; "Abraham" ]
    ; [ "apple"; "6"; "Bathsheba" ]
    ; [ "orange"; "2"; "Cyrus" ]
    ]
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "pretty"; "input.csv" ] in
    [%expect
      {|
            quantity
    fruit   |  owner
    |       |  |
    apple   4  Abraham
    apple   6  Bathsheba
    orange  2  Cyrus |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = system "csv pretty < input.csv" in
    [%expect
      {|
            quantity
    fruit   |  owner
    |       |  |
    apple   4  Abraham
    apple   6  Bathsheba
    orange  2  Cyrus |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "pretty"; "-sh"; "input.csv" ] in
    [%expect {|
    apple   4  Abraham
    apple   6  Bathsheba
    orange  2  Cyrus |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "pretty"; "-s"; "10"; "input.csv" ] in
    [%expect
      {|
                    quantity
    fruit           |          owner
    |               |          |
    apple           4          Abraham
    apple           6          Bathsheba
    orange          2          Cyrus |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "pretty"; "-d"; "."; "input.csv" ] in
    [%expect
      {|
    fruit,quantity,owner
    |
    apple,4,Abraham
    apple,6,Bathsheba
    orange,2,Cyrus |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () = run "csv" [ "pretty"; "-d"; "."; "input.csv" ] in
    [%expect
      {|
            quantity
    fruit   |  owner
    |       |  |
    apple   4  Abraham
    apple   6  Bathsheba
    orange  2  Cyrus |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      run ~enable_ocaml_backtraces:false "csv" [ "pretty"; "input.csv"; "input.csv" ]
    in
    [%expect
      {|
    ("Unclean exit" (Exit_non_zero 1))
    --- STDERR ---
    Error parsing command line:

      too many anonymous arguments

    For usage information, run

      csv pretty -help |}];
    return ())
;;
