open! Core
open Async

let run ?add_header ?sep reader writer =
  let reader =
    Delimited.Read.pipe_of_reader Delimited.Read.Row.builder ~header:`No ?sep reader
  in
  let writer = Delimited.Write.Expert.By_row.of_writer ?sep writer in
  Pipe.set_size_budget reader 1000;
  Pipe.set_size_budget writer 1000;
  Option.iter add_header ~f:(Pipe.write_without_pushback_if_open writer);
  Pipe.transfer reader writer ~f:Delimited.Read.Row.to_list
;;
