open! Core
open! Async
open! Import

let default_data =
  [ [ "string"; "int"; "float"; "time" ]
  ; [ "Abraham"; "1"; "1.1"; "0001-01-01T0000-00:00" ]
  ; [ "Bathsheba"; "2"; "2.2"; "0001-01-01T0500+10:00" ]
  ; [ "Cyrus"; "2"; "3.3"; "0002-01-01T0000-00:00" ]
  ; [ "Deborah"; "04"; "4.4"; "0003-01-01T0000-00:00" ]
  ; [ "Elijah"; "5"; "6.5"; "0004-01-01T0000-00:00" ]
  ]
;;

let make_input_csv ?sep ?(data = default_data) filename =
  make_input_csv ?sep filename data
;;

(* Sort on a field as a string. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      run "csv" [ "sort"; "-field"; "string"; "-field-type"; "string"; "input.csv" ]
    in
    [%expect
      {|
    string,int,float,time
    Abraham,1,1.1,0001-01-01T0000-00:00
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Deborah,04,4.4,0003-01-01T0000-00:00
    Elijah,5,6.5,0004-01-01T0000-00:00 |}];
    return ())
;;

(* Reversed string sort. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      run
        "csv"
        [ "sort"; "-field"; "string"; "-field-type"; "string"; "-reverse"; "input.csv" ]
    in
    [%expect
      {|
    string,int,float,time
    Elijah,5,6.5,0004-01-01T0000-00:00
    Deborah,04,4.4,0003-01-01T0000-00:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Abraham,1,1.1,0001-01-01T0000-00:00 |}];
    return ())
;;

(* Sort on an int field as an int. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      run "csv" [ "sort"; "-field"; "int"; "-field-type"; "int"; "input.csv" ]
    in
    [%expect
      {|
    string,int,float,time
    Abraham,1,1.1,0001-01-01T0000-00:00
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Deborah,04,4.4,0003-01-01T0000-00:00
    Elijah,5,6.5,0004-01-01T0000-00:00 |}];
    return ())
;;

(* Sort on an int field as a string. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "sort"; "-field"; "int"; "input.csv" ] in
    [%expect
      {|
    string,int,float,time
    Deborah,04,4.4,0003-01-01T0000-00:00
    Abraham,1,1.1,0001-01-01T0000-00:00
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Elijah,5,6.5,0004-01-01T0000-00:00 |}];
    return ())
;;

(* Reverse int sort.  Note the stability. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      run
        "csv"
        [ "sort"; "-field"; "int"; "-field-type"; "int"; "-reverse"; "input.csv" ]
    in
    [%expect
      {|
    string,int,float,time
    Elijah,5,6.5,0004-01-01T0000-00:00
    Deborah,04,4.4,0003-01-01T0000-00:00
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Abraham,1,1.1,0001-01-01T0000-00:00 |}];
    return ())
;;

(* Sort on a float field as a float. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      run "csv" [ "sort"; "-field"; "float"; "-field-type"; "float"; "input.csv" ]
    in
    [%expect
      {|
    string,int,float,time
    Abraham,1,1.1,0001-01-01T0000-00:00
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Deborah,04,4.4,0003-01-01T0000-00:00
    Elijah,5,6.5,0004-01-01T0000-00:00 |}];
    return ())
;;

(* Sort on a time field as a time. Note the time zone of Bathsheba, which breaks the usual
   equivalence between lexicographic and chronological ordering of ISO 8601 timestamps. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      run "csv" [ "sort"; "-field"; "time"; "-field-type"; "time"; "input.csv" ]
    in
    [%expect
      {|
    string,int,float,time
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Abraham,1,1.1,0001-01-01T0000-00:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Deborah,04,4.4,0003-01-01T0000-00:00
    Elijah,5,6.5,0004-01-01T0000-00:00 |}];
    return ())
;;

(* Delimiter. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () =
      run
        "csv"
        [ "sort"
        ; "-field"
        ; "string"
        ; "-field-type"
        ; "string"
        ; "-sep"
        ; "."
        ; "input.csv"
        ]
    in
    [%expect
      {|
    string.int.float.time
    Abraham.1."1.1".0001-01-01T0000-00:00
    Bathsheba.2."2.2".0001-01-01T0500+10:00
    Cyrus.2."3.3".0002-01-01T0000-00:00
    Deborah.04."4.4".0003-01-01T0000-00:00
    Elijah.5."6.5".0004-01-01T0000-00:00 |}];
    return ())
;;

(* natural sort requires some different data: *)

let%expect_test _ =
  do_test (fun () ->
    let data =
      [ [ "name_with_ints"; "value" ]
      ; [ "sys1"; "foo" ]
      ; [ "sys2"; "bar" ]
      ; [ "sys10"; "baz" ]
      ; [ "sys200"; "qwux" ]
      ]
    in
    let%bind () = make_input_csv ~data "input.csv" in
    let sort how =
      run "csv" [ "sort"; "-field"; "name_with_ints"; "-field-type"; how; "input.csv" ]
    in
    let%bind () = sort "string" in
    [%expect
      {|
      name_with_ints,value
      sys1,foo
      sys10,baz
      sys2,bar
      sys200,qwux |}];
    let%bind () = sort "natsort" in
    [%expect
      {|
      name_with_ints,value
      sys1,foo
      sys2,bar
      sys10,baz
      sys200,qwux |}];
    return ())
;;
