open Core

let max_length xs = List.fold xs ~init:1 ~f:(fun w x -> Int.max w (String.length x))

let string_padding x w =
  let w = Int.max 1 w in
  let n = String.length x in
  if w > n then Some (String.make (w - n) ' ') else None
;;

let pad_right x w =
  match string_padding x w with
  | None -> x
  | Some pad -> x ^ pad
;;

let pad_left x w =
  match string_padding x w with
  | None -> x
  | Some pad -> pad ^ x
;;

let matches of_string x =
  match of_string x with
  | _ -> true
  | exception _ -> false
;;

let may_be_numeric = function
  | "" -> true
  | n ->
    let n = Option.value ~default:n (String.chop_prefix ~prefix:"$" n) in
    matches Int.of_string n || matches Float.of_string n
;;

let col_type col = if List.for_all col ~f:may_be_numeric then `Number else `String

type t =
  { header_lines : string list
  ; row_lines : string list
  }

let lines t = t.header_lines @ t.row_lines

let print t =
  List.iter ~f:print_endline t.header_lines;
  List.iter ~f:print_endline t.row_lines
;;

let prettify_internal ~space ~suppress_header csv =
  let sep_width = space in
  let sep = String.make space ' ' in
  let header_block columns =
    List.rev
      (let hd, tl =
         List.fold_right
           columns
           ~init:("", [])
           ~f:(fun (header, column_width, col_type) (base_row, header_rows) ->
             let right_align =
               match col_type with
               | `String -> false
               | `Number -> true
             in
             let header_width = String.length header in
             let pad = String.make column_width ' ' in
             let bar_pad =
               let bar_place = if right_align then column_width - 1 else 0 in
               String.mapi pad ~f:(fun i c -> if i = bar_place then '|' else c)
             in
             let rec add = function
               | [] ->
                 let header =
                   if right_align && column_width > header_width
                   then String.make (column_width - header_width) ' ' ^ header
                   else header
                 in
                 [ header, 0 ]
               | (text, num_blanks) :: rest ->
                 if column_width + num_blanks >= header_width
                 then (
                   let pad text = pad ^ sep ^ text in
                   let text = Bytes.of_string (pad text) in
                   let dst_pos =
                     if right_align && column_width > header_width
                     then column_width - header_width
                     else 0
                   in
                   Bytes.From_string.blit
                     ~src:header
                     ~src_pos:0
                     ~dst:text
                     ~dst_pos
                     ~len:header_width;
                   (Bytes.to_string text, 0)
                   :: List.map rest ~f:(fun (text, num_blanks) ->
                     pad text, column_width + sep_width + num_blanks))
                 else (bar_pad ^ sep ^ text, 0) :: add rest
             in
             let base_row =
               if String.equal base_row ""
               then String.rstrip bar_pad
               else bar_pad ^ sep ^ base_row
             in
             base_row, add header_rows)
       in
       hd :: List.map ~f:fst tl)
  in
  match csv |> Csv_shape.create_verbose |> Csv_shape.to_error_string with
  | Error csv -> Error csv
  | Ok () ->
    let cols =
      csv
      |> List.transpose_exn
      |> List.map ~f:(function
        | [] -> assert false
        | header :: values ->
          let width = max_length values in
          let col_type = col_type values in
          let pad =
            match col_type with
            | `String -> pad_right
            | `Number -> pad_left
          in
          let header_info = header, width, col_type in
          header_info, List.map values ~f:(fun value -> pad value width))
    in
    let header_lines =
      if suppress_header
      then []
      else header_block (List.map cols ~f:(fun (header_info, _) -> header_info))
    in
    let row_lines =
      List.map
        ~f:(String.concat ~sep)
        (List.transpose_exn (List.map cols ~f:(fun (_, values) -> values)))
    in
    Ok { header_lines; row_lines }
;;

(* bootstrap *)
let prettify ~space ~suppress_header csv =
  match prettify_internal ~space ~suppress_header csv with
  | Ok result -> Ok result
  | Error error_csv ->
    let error_csv_lines =
      match prettify_internal ~space:2 ~suppress_header:false error_csv with
      | Ok t -> lines t
      | Error _ -> List.map error_csv ~f:(fun row -> String.concat ~sep:"," row)
    in
    Error
      (String.concat
         ~sep:"\n"
         ("Error: lines with different numbers of columns" :: "" :: error_csv_lines))
;;
