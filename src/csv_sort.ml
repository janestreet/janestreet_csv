open Core
open Csv_common
module Time = Time_float_unix

module Line_with_sort_key = struct
  type 'a t =
    { key : 'a
    ; line : string list
    }

  let compare c a b = c a.key b.key
  let line t = t.line
  let map_key t ~f = { t with key = f t.key }
end

module type Sortable = sig
  type t [@@deriving compare]

  val of_string : string -> t
end

module Converted = struct
  (* https://en.wikipedia.org/wiki/Schwartzian_transform *)

  type t =
    | T :
        { compare : 'a -> 'a -> int
        ; lines : 'a Line_with_sort_key.t Array.t
        }
        -> t

  let create_id compare lines = T { compare; lines }

  let create (module M : Sortable) lines =
    let lines = Array.map lines ~f:(Line_with_sort_key.map_key ~f:M.of_string) in
    create_id M.compare lines
  ;;

  (* The order here kinda matters, at least in that we need to check Int before Float.
     (2^63 is a parsable float, but loses information in the process.)

     All the typed values, however, are obviously incompatible (i.e. we can't confuse
     a time/span/byte, since the suffixes are unambiguous.) *)
  let infer_choices : (module Sortable) list =
    [ (module Time_ns.Span)
    ; (module Byte_units)
    ; (module Time)
    ; (module Int)
    ; (module Float)
    ]
  ;;

  let create_inferred lines =
    List.find_map infer_choices ~f:(fun choice ->
      Option.try_with (fun () -> create choice lines))
    (* Can't default to natsort because it would change behavior. Sad. *)
    |> Option.value_or_thunk ~default:(fun () -> create_id String.compare lines)
  ;;
end

module Sort_type = struct
  module T = struct
    type t =
      | Bytes
      | Float
      | Infer
      | Int
      | Natsort
      | Span
      | String
      | Time
    [@@deriving compare, enumerate, sexp_of]
  end

  include T

  let param =
    Enum.make_param_optional_with_default_doc
      "-field-type"
      (module T)
      ~aliases:[ "--field-type" ]
      ~default:Infer
      ~doc:"field type for sorting"
      ~represent_choice_with:"_"
  ;;

  let convert sort_type (lines : string Line_with_sort_key.t Array.t) =
    match sort_type with
    | Bytes -> Converted.create (module Byte_units) lines
    | Float -> Converted.create (module Float) lines
    | Infer -> Converted.create_inferred lines
    | Int -> Converted.create (module Int) lines
    | Natsort -> Converted.create_id Numeric_string.compare lines
    | Span -> Converted.create (module Time_ns.Span) lines
    | String -> Converted.create_id String.compare lines
    | Time -> Converted.create (module Time) lines
  ;;
end

let sort_on_field ~sort_type ~field ~reverse csv =
  match List.findi csv.header ~f:(fun _idx elem -> String.( = ) elem field) with
  | None -> failwithf "unable to find csv field %s" field ()
  | Some (idx, _) ->
    let lines =
      Array.of_list_map csv.lines ~f:(fun line ->
        { Line_with_sort_key.key = List.nth_exn line idx; line })
    in
    let (T { lines; compare }) = Sort_type.convert sort_type lines in
    let compare =
      if reverse then fun a b -> Comparable.reverse compare a b else compare
    in
    let compare = Line_with_sort_key.compare compare in
    Array.stable_sort lines ~compare;
    let lines = Array.map lines ~f:Line_with_sort_key.line |> Array.to_list in
    { header = csv.header; lines }
;;

let run ?separator ?(reverse = false) ~sort_type ~field file =
  Or_file.with_all file ?separator ~f:(fun csv ->
    csv |> sort_on_field ~sort_type ~field ~reverse |> print_csv ?separator)
;;
