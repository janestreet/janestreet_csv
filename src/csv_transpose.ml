open Core

let run ?separator file =
  Csv_common.Or_file.with_all file ?separator ~f:(fun (csv : Csv_common.t) ->
    Csvlib.Csv.print ?separator (List.transpose_exn (csv.header :: csv.lines)))
;;
