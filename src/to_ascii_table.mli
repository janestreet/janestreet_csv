val run
  :  ?separator:char
  -> ?limit_width_to:int
  -> ?max_col_width:int
  -> ?prefer_split_on_spaces:bool
  -> Csv_common.Or_file.t
  -> unit
