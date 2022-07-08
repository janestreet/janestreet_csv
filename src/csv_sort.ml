open Core
open Csv_common
module Time = Time_float_unix

module Sort_type = struct
  module T = struct
    type t =
      | Float
      | Int
      | Natsort
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
      ~default:String
      ~doc:"field type for sorting"
      ~represent_choice_with:"_"
  ;;

  let compare_string t a b =
    match t with
    | Natsort -> Numeric_string.compare a b
    | String -> String.compare a b
    | Int -> Int.compare (Int.of_string a) (Int.of_string b)
    | Time -> Time.compare (Time.of_string a) (Time.of_string b)
    | Float -> Float.compare (Float.of_string a) (Float.of_string b)
  ;;
end

let compare ~reverse sort_type a b =
  let result = Sort_type.compare_string sort_type a b in
  if reverse then -1 * result else result
;;

let sort_on_field ~sort_type ~field ~reverse csv =
  match List.findi csv.header ~f:(fun _idx elem -> String.( = ) elem field) with
  | None -> failwithf "unable to find csv field %s" field ()
  | Some (idx, _) ->
    let lines =
      List.stable_sort csv.lines ~compare:(fun a b ->
        compare ~reverse sort_type (List.nth_exn a idx) (List.nth_exn b idx))
    in
    { header = csv.header; lines }
;;

let run ?separator ?(reverse = false) ~sort_type ~field file =
  Or_file.with_all file ?separator ~f:(fun csv ->
    csv |> sort_on_field ~sort_type ~field ~reverse |> print_csv ?separator)
;;
