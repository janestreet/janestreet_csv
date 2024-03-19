open! Core
open! Async
open! Import

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
    let%bind () = run ~enable_ocaml_backtraces:false "csv" [ "to-sexp"; "input.csv" ] in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      Error parsing command line:

        too many anonymous arguments

      For usage information, run

        csv to-sexp -help
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = system "csv to-sexp < input.csv" in
    [%expect
      {|
      ((fruit apple)(quantity 4)(owner Abraham))
      ((fruit apple)(quantity 6)(owner Bathsheba))
      ((fruit orange)(quantity 2)(owner Cyrus))
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = system "csv to-sexp -no-header < input.csv" in
    [%expect
      {|
      (fruit quantity owner)
      (apple 4 Abraham)
      (apple 6 Bathsheba)
      (orange 2 Cyrus)
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "to-sexp"; "-nh" ] in
    [%expect {| |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () = system "csv to-sexp -d . < input.csv" in
    [%expect
      {|
      ((fruit apple)(quantity 4)(owner Abraham))
      ((fruit apple)(quantity 6)(owner Bathsheba))
      ((fruit orange)(quantity 2)(owner Cyrus))
      |}];
    return ())
;;
