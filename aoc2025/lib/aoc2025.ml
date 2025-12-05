let read_input_lines (filename : string) : string list =
  In_channel.with_open_text filename In_channel.input_lines

let read_input_all (filename : string) : string =
  In_channel.with_open_text filename In_channel.input_all
