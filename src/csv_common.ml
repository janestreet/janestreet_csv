open Core
module Csv = Csvlib.Csv

type t =
  { header : string list
  ; lines : string list list
  }
[@@deriving compare, sexp]

let empty = { header = []; lines = [] }
let print_csv ?separator t = Csv.print ?separator (t.header :: t.lines)

let of_csvlib_csv = function
  | [] -> failwith "missing csv header"
  | header :: lines -> { header; lines }
;;

let load ?separator file = of_csvlib_csv (Csv.load ?separator file)
let load_all ?separator files = List.map files ~f:(load ?separator)

module Or_file = struct
  type nonrec t =
    | Csv of t
    | File of Filename.t
    | Stdin
  [@@deriving compare, sexp_of]

  include struct
    open Command.Param

    let arg_type =
      Arg_type.map Filename_unix.arg_type ~f:(fun filename ->
        if String.( = ) filename "-" then Stdin else File filename)
    ;;

    let default = Stdin
    let anon = anon (maybe_with_default default ("filename" %: arg_type))

    let flag =
      flag
        "-file"
        (optional_with_default default arg_type)
        ~doc:"FILE read a files instead of stdin"
    ;;
  end

  let with_in_channel ?separator in_channel ~f =
    protectx in_channel ~finally:In_channel.close ~f:(fun ic ->
      f (of_csvlib_csv (Csv.load_in ?separator ic)))
  ;;

  let with_all ?separator t ~f =
    match t with
    | Csv csv -> f csv
    | File x -> with_in_channel ?separator (In_channel.create x) ~f
    | Stdin -> with_in_channel ?separator In_channel.stdin ~f
  ;;

  let with_in_channel_lines ?separator in_channel ~header_f ~f =
    let print_csv_line line = Csv.print ?separator [ line ] in
    let header = ref None in
    let wrap_f line =
      match !header with
      | None ->
        let new_header, state = header_f line in
        header := Some (new_header, state);
        print_csv_line new_header
      | Some (header, state) -> print_csv_line (f ~header ~state line)
    in
    protectx
      in_channel
      ~f:(fun ic -> Csv.load_rows ?separator wrap_f ic)
      ~finally:In_channel.close
  ;;

  let with_lines ?separator t ~header_f ~f =
    match t with
    | Csv { header; lines } ->
      let header, state = header_f header in
      Csv.print ?separator (header :: List.map lines ~f:(f ~header ~state))
    | Stdin | File "-" -> with_in_channel_lines ?separator In_channel.stdin ~header_f ~f
    | File x -> with_in_channel_lines ?separator (In_channel.create x) ~header_f ~f
  ;;
end
