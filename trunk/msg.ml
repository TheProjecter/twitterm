open ANSITerminal

let msg style = fun str -> (print_newline (); print_string style str; flush_all ())

let normal_msg str = msg [] str

let error_msg str = msg [Bold; red] str

let admin_msg str = msg [blue] str

let prompt_msg str = msg [yellow; on_blue] str

let tweet_msg usr msg = 
  print_newline ();
  print_string [Bold; blue] usr;
  print_string [] ": ";
  print_string msg;
  flush_all ()
