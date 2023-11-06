open! Core

module Order : sig
  type t =
    | Ascending
    | Descending
  [@@deriving compare, enumerate, sexp_of]
end

module Sort_type : sig
  type t =
    | Bytes
    | Float
    | Infer
    | Int
    | Natsort (** https://en.wikipedia.org/wiki/Natural_sort_order *)
    | Span
    | String
    | Time
  [@@deriving compare, enumerate, sexp_of]
end

module Sort_column : sig
  type t =
    { field : string
    ; order : Order.t
    ; sort_type : Sort_type.t
    }
  [@@deriving sexp_of]

  val param : t list Command.Param.t
end

val sort_on_fields : Sort_column.t list -> Csv_common.t -> Csv_common.t
val run : ?separator:char -> Sort_column.t list -> Csv_common.Or_file.t -> unit
