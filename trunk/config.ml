open Unix
open Msg

let touch name mode = close( openfile name [O_RDWR; O_CREAT] mode )

let ensure_file name is_dir =
  try access name [F_OK]
  with Unix_error -> (
    admin_msg "Creating file: " ^ name;
    let mkfun, mode = 
      if is_dir then mkdir, 0o700
      else touch, 0o600
    in
    mkfun name mode
  )

let first_line filename = 
  let chan = open_in filename in
  let out = input_line chan in
  close_in chan;
  out

let config_dir =
  let home = try getenv "HOME" with Not_found -> "." in
  let name = Sys.executable_name in
  let out = home ^ "/." ^ name in
  ensure_file config_dir true;
  out

let config_file name =
  let name = config_dir ^ "/" ^ name in
  ensure_file name false;
  name

let write_convar name value = 
  let chan = open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_text] 0o600 name in
  output_string chan value;
  close_out chan;
  ()
  

let convar name = let name = config_file name in first_line name

let username = convar "username"

let password = convar "password"
