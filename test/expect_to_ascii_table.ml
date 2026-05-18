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
    let%bind () = run "csv" [ "to-ascii-table"; "input.csv" ] in
    [%expect
      {|
      ┌────────┬──────────┬───────────┐
      │  fruit │ quantity │     owner │
      ├────────┼──────────┼───────────┤
      │  apple │        4 │   Abraham │
      │  apple │        6 │ Bathsheba │
      │ orange │        2 │     Cyrus │
      └────────┴──────────┴───────────┘
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "to-ascii-table"; "-limit-width-to"; "30"; "input.csv" ] in
    [%expect
      {|
      ┌────────┬────────┬──────────┐
      │  fruit │ quanti │    owner │
      │        │     ty │          │
      ├────────┼────────┼──────────┤
      │  apple │      4 │  Abraham │
      │  apple │      6 │ Bathsheb │
      │        │        │        a │
      │ orange │      2 │    Cyrus │
      └────────┴────────┴──────────┘
      |}];
    return ())
;;

let%expect_test "max-column-width" =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "to-ascii-table"; "-max-column-width"; "5"; "input.csv" ] in
    [%expect
      {|
      ┌─────┬─────┬─────┐
      │ fru │ qua │ own │
      │  it │ nti │  er │
      │     │  ty │     │
      ├─────┼─────┼─────┤
      │ app │   4 │ Abr │
      │  le │     │ aha │
      │     │     │   m │
      │ app │   6 │ Bat │
      │  le │     │ hsh │
      │     │     │ eba │
      │ ora │   2 │ Cyr │
      │ nge │     │  us │
      └─────┴─────┴─────┘
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () = run "csv" [ "to-ascii-table"; "-sep"; "."; "input.csv" ] in
    [%expect
      {|
      ┌────────┬──────────┬───────────┐
      │  fruit │ quantity │     owner │
      ├────────┼──────────┼───────────┤
      │  apple │        4 │   Abraham │
      │  apple │        6 │ Bathsheba │
      │ orange │        2 │     Cyrus │
      └────────┴──────────┴───────────┘
      |}];
    return ())
;;
