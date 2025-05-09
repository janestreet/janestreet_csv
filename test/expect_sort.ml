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

let sort_test ?sep ?data ?field ?field_type flags =
  let flags =
    match field_type with
    | None -> flags
    | Some t -> "-field-type" :: t :: flags
  in
  let flags =
    match field with
    | None -> flags
    | Some t -> "-field" :: t :: flags
  in
  do_test (fun () ->
    let%bind () = make_input_csv ?sep ?data "input.csv" in
    run "csv" ("sort" :: "input.csv" :: flags))
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
    Elijah,5,6.5,0004-01-01T0000-00:00
    |}];
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
    Abraham,1,1.1,0001-01-01T0000-00:00
    |}];
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
    Elijah,5,6.5,0004-01-01T0000-00:00
    |}];
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
    Elijah,5,6.5,0004-01-01T0000-00:00
    |}];
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
    Abraham,1,1.1,0001-01-01T0000-00:00
    |}];
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
    Elijah,5,6.5,0004-01-01T0000-00:00
    |}];
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
    Elijah,5,6.5,0004-01-01T0000-00:00
    |}];
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
    Elijah.5."6.5".0004-01-01T0000-00:00
    |}];
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
    sys200,qwux
    |}];
  let%bind () = sort "natsort" in
  [%expect
    {|
    name_with_ints,value
    sys1,foo
    sys2,bar
    sys10,baz
    sys200,qwux
    |}];
  return ()
;;

let%expect_test "bytes, spans" =
  let data =
    [ [ "latency"; "size" ]; [ "2h"; "17b" ]; [ "1ns"; "1K" ]; [ "3us"; "1.2G" ] ]
  in
  let sort field field_type = sort_test ~field ~field_type ~data [] in
  let%bind () = sort "latency" "span" in
  [%expect
    {|
    latency,size
    1ns,1K
    3us,1.2G
    2h,17b
    |}];
  let%bind () = sort "size" "bytes" in
  [%expect
    {|
    latency,size
    2h,17b
    1ns,1K
    3us,1.2G
    |}];
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
    Deferred.List.iter ~how:`Sequential headers ~f:(fun field ->
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
    c,2,2m,0.17e4
    |}];
  return ()
;;

let multi_column_headers = [ "string"; "int"; "bytes"; "float" ]

let multi_column_data =
  [ multi_column_headers
    (* 1g vs 3k in bytes column chosen to detect if we're just ignoring the suffix. *)
  ; [ "b"; "2"; "1g"; "0.17e4" ]
  ; [ "b"; "1"; "1g"; "0.17e4" ]
  ; [ "b"; "1"; "1g"; "   2.5" ]
  ; [ "a"; "1"; "1g"; "   2.5" ]
  ; [ "a"; "2"; "3k"; "0.17e4" ]
  ; [ "a"; "1"; "3k"; "0.17e4" ]
  ; [ "b"; "1"; "3k"; "   2.5" ]
  ; [ "a"; "1"; "1g"; "0.17e4" ]
  ; [ "a"; "1"; "3k"; "   2.5" ]
  ; [ "b"; "2"; "3k"; "0.17e4" ]
  ; [ "b"; "1"; "3k"; "0.17e4" ]
  ; [ "a"; "2"; "1g"; "0.17e4" ]
  ; [ "a"; "2"; "1g"; "   2.5" ]
  ; [ "b"; "2"; "1g"; "   2.5" ]
  ; [ "a"; "2"; "3k"; "   2.5" ]
  ; [ "b"; "2"; "3k"; "   2.5" ]
  ]
;;

let multi_column_sort_test ~extra_args =
  let headers = multi_column_headers in
  let data = multi_column_data in
  List.cartesian_product headers headers
  |> List.filter ~f:(fun (x, y) -> String.(x <> y))
  |> Deferred.List.iter ~how:`Sequential ~f:(fun fields ->
    let comma_separated_fields = [%string "%{fst fields},%{snd fields}"] in
    print_newline ();
    print_endline [%string "sorted by %{comma_separated_fields}\n================="];
    sort_test ~data ~field:comma_separated_fields (extra_args ~fields))
;;

let%expect_test "multi-column" =
  let%bind () = multi_column_sort_test ~extra_args:(fun ~fields:_ -> []) in
  [%expect
    {|
    sorted by string,int
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,1,3k,   2.5
    a,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,1,3k,0.17e4
    b,2,1g,0.17e4
    b,2,3k,0.17e4
    b,2,1g,   2.5
    b,2,3k,   2.5

    sorted by string,bytes
    =================
    string,int,bytes,float
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,3k,   2.5
    a,2,3k,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,1,3k,   2.5
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,3k,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,2,1g,   2.5

    sorted by string,float
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    a,2,3k,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,2,1g,   2.5
    b,2,3k,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4

    sorted by int,string
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,1,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,1,3k,0.17e4
    a,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,2,1g,0.17e4
    b,2,3k,0.17e4
    b,2,1g,   2.5
    b,2,3k,   2.5

    sorted by int,bytes
    =================
    string,int,bytes,float
    a,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,3k,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,2,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5

    sorted by int,float
    =================
    string,int,bytes,float
    b,1,1g,   2.5
    a,1,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,1g,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    b,1,3k,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,2,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,1g,0.17e4

    sorted by bytes,string
    =================
    string,int,bytes,float
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,1,3k,   2.5
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,3k,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,2,1g,   2.5

    sorted by bytes,int
    =================
    string,int,bytes,float
    a,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,3k,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    b,2,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5

    sorted by bytes,float
    =================
    string,int,bytes,float
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4

    sorted by float,string
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,2,1g,   2.5
    b,2,3k,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4

    sorted by float,int
    =================
    string,int,bytes,float
    b,1,1g,   2.5
    a,1,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,1,1g,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    b,1,3k,0.17e4
    b,2,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,1g,0.17e4

    sorted by float,bytes
    =================
    string,int,bytes,float
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    |}];
  return ()
;;

let%expect_test "multi-column-all-rev" =
  let%bind () = multi_column_sort_test ~extra_args:(fun ~fields:_ -> [ "-reverse" ]) in
  [%expect
    {|
    sorted by string,int
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    b,2,3k,0.17e4
    b,2,1g,   2.5
    b,2,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,1,3k,0.17e4
    a,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,   2.5
    a,1,1g,   2.5
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,1,3k,   2.5

    sorted by string,bytes
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,2,1g,   2.5
    b,1,3k,   2.5
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,3k,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,3k,   2.5
    a,2,3k,   2.5

    sorted by string,float
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,2,1g,   2.5
    b,2,3k,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,1,1g,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    a,2,3k,   2.5

    sorted by int,string
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    b,2,3k,0.17e4
    b,2,1g,   2.5
    b,2,3k,   2.5
    a,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,1,3k,0.17e4
    a,1,1g,   2.5
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,1,3k,   2.5

    sorted by int,bytes
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,3k,0.17e4

    sorted by int,float
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,1,1g,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    b,1,3k,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5

    sorted by bytes,string
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,2,1g,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,1,3k,   2.5
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,3k,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,3k,   2.5
    a,2,3k,   2.5

    sorted by bytes,int
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,3k,   2.5
    b,2,3k,   2.5
    a,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,3k,0.17e4

    sorted by bytes,float
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5

    sorted by float,string
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,2,1g,   2.5
    b,2,3k,   2.5
    a,1,1g,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    a,2,3k,   2.5

    sorted by float,int
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,1g,0.17e4
    b,1,1g,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    b,1,3k,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,1,1g,   2.5
    a,1,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5

    sorted by float,bytes
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    |}];
  return ()
;;

let%expect_test "multi-column-one-rev" =
  let%bind () =
    multi_column_sort_test ~extra_args:(fun ~fields:(x, _) -> [ "-reverse-fields"; x ])
  in
  [%expect
    {|
    sorted by string,int
    =================
    string,int,bytes,float
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,1,3k,0.17e4
    b,2,1g,0.17e4
    b,2,3k,0.17e4
    b,2,1g,   2.5
    b,2,3k,   2.5
    a,1,1g,   2.5
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,1,3k,   2.5
    a,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,   2.5

    sorted by string,bytes
    =================
    string,int,bytes,float
    b,1,3k,   2.5
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,3k,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,3k,   2.5
    a,2,3k,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5

    sorted by string,float
    =================
    string,int,bytes,float
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,2,1g,   2.5
    b,2,3k,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    a,1,1g,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    a,2,3k,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4

    sorted by int,string
    =================
    string,int,bytes,float
    a,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,2,1g,0.17e4
    b,2,3k,0.17e4
    b,2,1g,   2.5
    b,2,3k,   2.5
    a,1,1g,   2.5
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,1,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,1,3k,0.17e4

    sorted by int,bytes
    =================
    string,int,bytes,float
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,2,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,3k,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4

    sorted by int,float
    =================
    string,int,bytes,float
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,2,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,1g,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    b,1,3k,0.17e4

    sorted by bytes,string
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,1,3k,   2.5
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,3k,   2.5

    sorted by bytes,int
    =================
    string,int,bytes,float
    b,1,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    b,2,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,3k,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,3k,   2.5
    b,2,3k,   2.5

    sorted by bytes,float
    =================
    string,int,bytes,float
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4

    sorted by float,string
    =================
    string,int,bytes,float
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    a,1,1g,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,2,1g,   2.5
    b,2,3k,   2.5

    sorted by float,int
    =================
    string,int,bytes,float
    b,1,1g,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    b,1,3k,0.17e4
    b,2,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5

    sorted by float,bytes
    =================
    string,int,bytes,float
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    |}];
  return ()
;;

let%expect_test "multi-column-sort-types" =
  let%bind () =
    multi_column_sort_test ~extra_args:(fun ~fields:_ ->
      [ "-field-types"; "string,natsort" ])
  in
  [%expect
    {|
    sorted by string,int
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,1,3k,   2.5
    a,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,1,3k,0.17e4
    b,2,1g,0.17e4
    b,2,3k,0.17e4
    b,2,1g,   2.5
    b,2,3k,   2.5

    sorted by string,bytes
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,2,1g,   2.5
    b,1,3k,   2.5
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,3k,   2.5

    sorted by string,float
    =================
    string,int,bytes,float
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,1,1g,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,2,1g,   2.5
    b,2,3k,   2.5

    sorted by int,string
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,1,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,1,3k,0.17e4
    a,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,2,1g,0.17e4
    b,2,3k,0.17e4
    b,2,1g,   2.5
    b,2,3k,   2.5

    sorted by int,bytes
    =================
    string,int,bytes,float
    b,1,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,3k,0.17e4
    b,2,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,3k,   2.5
    b,2,3k,   2.5

    sorted by int,float
    =================
    string,int,bytes,float
    b,1,1g,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    b,1,3k,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,2,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5

    sorted by bytes,string
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,1,3k,   2.5
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,3k,   2.5

    sorted by bytes,int
    =================
    string,int,bytes,float
    b,1,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    b,2,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,3k,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,3k,   2.5
    b,2,3k,   2.5

    sorted by bytes,float
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5

    sorted by float,string
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,2,1g,   2.5
    b,2,3k,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4

    sorted by float,int
    =================
    string,int,bytes,float
    b,1,1g,   2.5
    a,1,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,1,1g,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    b,1,3k,0.17e4
    b,2,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,1g,0.17e4

    sorted by float,bytes
    =================
    string,int,bytes,float
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    |}];
  return ()
;;

let%expect_test "no fields" =
  let%bind () = sort_test ~data:multi_column_data [] in
  [%expect
    {|
    string,int,bytes,float
    a,1,3k,   2.5
    a,1,3k,0.17e4
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,3k,   2.5
    a,2,3k,0.17e4
    a,2,1g,   2.5
    a,2,1g,0.17e4
    b,1,3k,   2.5
    b,1,3k,0.17e4
    b,1,1g,   2.5
    b,1,1g,0.17e4
    b,2,3k,   2.5
    b,2,3k,0.17e4
    b,2,1g,   2.5
    b,2,1g,0.17e4
    |}];
  return ()
;;

let%expect_test "no fields reversed" =
  let%bind () = sort_test ~data:multi_column_data [ "-reverse" ] in
  [%expect
    {|
    string,int,bytes,float
    b,2,1g,0.17e4
    b,2,1g,   2.5
    b,2,3k,0.17e4
    b,2,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,0.17e4
    b,1,3k,   2.5
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,0.17e4
    a,2,3k,   2.5
    a,1,1g,0.17e4
    a,1,1g,   2.5
    a,1,3k,0.17e4
    a,1,3k,   2.5
    |}];
  return ()
;;
