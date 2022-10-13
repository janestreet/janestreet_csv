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

let sort_test ?sep ?data ~field ?field_type flags =
  let ft =
    match field_type with
    | None -> []
    | Some t -> [ "-field-type"; t ]
  in
  do_test (fun () ->
    let%bind () = make_input_csv ?sep ?data "input.csv" in
    run "csv" ([ "sort"; "-field"; field ] @ ft @ [ "input.csv" ] @ flags))
;;

(* Sort on a field as a string. *)
let%expect_test _ =
  let%bind () = sort_test ~field:"string" ~field_type:"string" [] in
  [%expect
    {|
    string,int,float,time
    Abraham,1,1.1,0001-01-01T0000-00:00
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Deborah,04,4.4,0003-01-01T0000-00:00
    Elijah,5,6.5,0004-01-01T0000-00:00 |}];
  return ()
;;

(* Reversed string sort. *)
let%expect_test _ =
  let%bind () = sort_test ~field:"string" ~field_type:"string" [ "-reverse" ] in
  [%expect
    {|
    string,int,float,time
    Elijah,5,6.5,0004-01-01T0000-00:00
    Deborah,04,4.4,0003-01-01T0000-00:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Abraham,1,1.1,0001-01-01T0000-00:00 |}];
  return ()
;;

(* Sort on an int field as an int. *)
let%expect_test _ =
  let%bind () = sort_test ~field:"int" ~field_type:"int" [] in
  [%expect
    {|
    string,int,float,time
    Abraham,1,1.1,0001-01-01T0000-00:00
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Deborah,04,4.4,0003-01-01T0000-00:00
    Elijah,5,6.5,0004-01-01T0000-00:00 |}];
  return ()
;;

(* Sort on an int field as a string. *)
let%expect_test _ =
  let%bind () = sort_test ~field:"int" ~field_type:"string" [] in
  [%expect
    {|
    string,int,float,time
    Deborah,04,4.4,0003-01-01T0000-00:00
    Abraham,1,1.1,0001-01-01T0000-00:00
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Elijah,5,6.5,0004-01-01T0000-00:00 |}];
  return ()
;;

(* Reverse int sort.  Note the stability. *)
let%expect_test _ =
  let%bind () = sort_test ~field:"int" ~field_type:"int" [ "-reverse" ] in
  [%expect
    {|
    string,int,float,time
    Elijah,5,6.5,0004-01-01T0000-00:00
    Deborah,04,4.4,0003-01-01T0000-00:00
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Abraham,1,1.1,0001-01-01T0000-00:00 |}];
  return ()
;;

(* Sort on a float field as a float. *)
let%expect_test _ =
  let%bind () = sort_test ~field:"float" ~field_type:"float" [] in
  [%expect
    {|
    string,int,float,time
    Abraham,1,1.1,0001-01-01T0000-00:00
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Deborah,04,4.4,0003-01-01T0000-00:00
    Elijah,5,6.5,0004-01-01T0000-00:00 |}];
  return ()
;;

(* Sort on a time field as a time. Note the time zone of Bathsheba, which breaks the usual
   equivalence between lexicographic and chronological ordering of ISO 8601 timestamps. *)
let%expect_test _ =
  let%bind () = sort_test ~field:"time" ~field_type:"time" [] in
  [%expect
    {|
    string,int,float,time
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Abraham,1,1.1,0001-01-01T0000-00:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Deborah,04,4.4,0003-01-01T0000-00:00
    Elijah,5,6.5,0004-01-01T0000-00:00 |}];
  return ()
;;

(* Delimiter. *)
let%expect_test _ =
  let%bind () = sort_test ~field:"string" ~field_type:"string" ~sep:'.' [ "-sep"; "." ] in
  [%expect
    {|
    string.int.float.time
    Abraham.1."1.1".0001-01-01T0000-00:00
    Bathsheba.2."2.2".0001-01-01T0500+10:00
    Cyrus.2."3.3".0002-01-01T0000-00:00
    Deborah.04."4.4".0003-01-01T0000-00:00
    Elijah.5."6.5".0004-01-01T0000-00:00 |}];
  return ()
;;

(* natural sort requires some different data: *)

let%expect_test _ =
  let data =
    [ [ "name_with_ints"; "value" ]
    ; [ "sys1"; "foo" ]
    ; [ "sys2"; "bar" ]
    ; [ "sys10"; "baz" ]
    ; [ "sys200"; "qwux" ]
    ]
  in
  let sort how = sort_test ~data ~field:"name_with_ints" ~field_type:how [] in
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
  return ()
;;

let%expect_test "bytes, spans" =
  let data =
    [ [ "latency"; "size" ]; [ "2h"; "17b" ]; [ "1ns"; "1K" ]; [ "3us"; "1.2G" ] ]
  in
  let sort field field_type = sort_test ~field ~field_type ~data [] in
  let%bind () = sort "latency" "span" in
  [%expect {|
    latency,size
    1ns,1K
    3us,1.2G
    2h,17b |}];
  let%bind () = sort "size" "bytes" in
  [%expect {|
    latency,size
    2h,17b
    1ns,1K
    3us,1.2G |}];
  return ()
;;

let%expect_test "inference" =
  let headers = [ "string"; "int"; "bytes"; "float" ] in
  let data =
    (* bytes column chosen to detect if we're just ignoring the suffix. *)
    [ headers
    ; [ "a"; "4"; "1g"; "3" ]
    ; [ "d"; "5"; "3k"; "2.5" ]
    ; [ "c"; "2"; "2m"; "0.17e4" ]
    ]
  in
  let%bind () =
    Deferred.List.iter headers ~f:(fun field ->
      print_newline ();
      print_endline [%string "sorted by %{field}\n================="];
      sort_test ~data ~field [])
  in
  [%expect
    {|
    sorted by string
    =================
    string,int,bytes,float
    a,4,1g,3
    c,2,2m,0.17e4
    d,5,3k,2.5

    sorted by int
    =================
    string,int,bytes,float
    c,2,2m,0.17e4
    a,4,1g,3
    d,5,3k,2.5

    sorted by bytes
    =================
    string,int,bytes,float
    d,5,3k,2.5
    c,2,2m,0.17e4
    a,4,1g,3

    sorted by float
    =================
    string,int,bytes,float
    d,5,3k,2.5
    a,4,1g,3
    c,2,2m,0.17e4 |}];
  return ()
;;
