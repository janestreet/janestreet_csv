open Core
module Csv = Csvlib.Csv

exception Not_enough_rows

(*exception Invalid_range of string list*)

type row_processor =
  Csv_common.Or_file.t
  -> skip_header:bool
  -> sep:char
  -> f:(string list -> unit)
  -> [ `Limit_to of string list | `All_but of string list ]
  -> unit

let load_rows file ~sep ~f =
  let process ic =
    protectx ic ~f:(fun ic -> Csv.load_rows ~separator:sep f ic) ~finally:In_channel.close
  in
  match (file : Csv_common.Or_file.t) with
  | Csv { header; lines } -> List.iter (header :: lines) ~f
  | Stdin | File "-" -> process In_channel.stdin
  | File x -> process (In_channel.create x)
;;

let get_positions header_row headers_wanted =
  match headers_wanted with
  | `Limit_to headers ->
    let hmap = String.Map.of_alist_exn (List.mapi header_row ~f:(fun i h -> h, i)) in
    List.map headers ~f:(fun header ->
      match Map.find hmap header with
      | Some position -> position
      | None ->
        (try Int.of_string header with
         | _ -> raise_s [%message "Unknown header" ~_:(header : string)]))
  | `All_but headers ->
    let headers = String.Set.of_list headers in
    List.filter_mapi header_row ~f:(fun pos header ->
      if Set.mem headers header then None else Some pos)
;;

let cut_by_field_names file ~skip_header ~sep ~f headers_wanted =
  let handle_row = ref (fun _ -> assert false) in
  (handle_row
   := fun row ->
     let positions = get_positions row headers_wanted in
     let grab row =
       List.map positions ~f:(fun i ->
         match List.nth row i with
         | None -> ""
         | Some x -> x)
     in
     if not skip_header then f (grab row);
     handle_row := fun row -> f (grab row));
  load_rows file ~sep ~f:(fun row -> !handle_row row)
;;

exception First_row of string list

let field_names ~sep file =
  let throw_row row = raise (First_row row) in
  try
    load_rows file ~sep ~f:throw_row;
    (* should throw First_row before this! *)
    raise Not_enough_rows
  with
  | First_row row -> row
;;

let regex_match file ~sep ~f ~regexp =
  let handle_row rows =
    let grab rows =
      List.map rows ~f:(fun row -> if Pcre.pmatch ~rex:regexp row then row else "")
    in
    f (grab rows)
  in
  load_rows file ~sep ~f:handle_row
;;

(* apply [f_populated] to rows that have all fields corresponding to [headers] populated
   apply [f_unpopulated] to all other rows
*)
let split_populated_rows file ~skip_header ~sep ~f_populated ~f_unpopulated headers_wanted
  =
  let handle_row = ref (fun _ -> assert false) in
  (handle_row
   := fun row ->
     let positions = get_positions row headers_wanted in
     let populated row =
       List.for_all positions ~f:(fun i ->
         match List.nth row i with
         | None | Some "" -> false
         | Some _ -> true)
     in
     if not skip_header
     then (
       f_populated row;
       f_unpopulated row);
     handle_row
     := fun row -> if populated row then f_populated row else f_unpopulated row);
  load_rows file ~sep ~f:(fun row -> !handle_row row)
;;

let fully_populated_rows file ~skip_header ~sep ~f headers_wanted =
  split_populated_rows
    file
    ~skip_header
    ~sep
    ~f_populated:f
    ~f_unpopulated:ignore
    headers_wanted
;;

let not_fully_populated_rows file ~skip_header ~sep ~f headers_wanted =
  split_populated_rows
    file
    ~skip_header
    ~sep
    ~f_populated:ignore
    ~f_unpopulated:f
    headers_wanted
;;
