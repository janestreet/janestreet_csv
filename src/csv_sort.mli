open! Core

module Sort_type : sig
  type t =
    | Float
    | Int
    | Natsort (** https://en.wikipedia.org/wiki/Natural_sort_order *)
    | String
    | Time
  [@@deriving compare, enumerate, sexp_of]

  val param : t Command.Param.t
end

val sort_on_field
  :  sort_type:Sort_type.t
  -> field:string
  -> reverse:bool
  -> Csv_common.t
  -> Csv_common.t

val run
  :  ?separator:char
  -> ?reverse:bool
  -> sort_type:Sort_type.t
  -> field:string
  -> Csv_common.Or_file.t
  -> unit
