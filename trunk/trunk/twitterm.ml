open Sys
open ANSITerminal
open Msg
open TwitApi

let prompt_for_input () = prompt_msg ":>"; read_line ()

let send_to_twitter str = admin_msg ("Sent to twitter: " ^ str)

let update_tweets() =
  let report_error x = 
    let err_str = 
      match x with
        | NotModified -> None
        | Unknown(i,m) -> Some("Unknown Error: " ^ (string_of_int i) ^ " " ^ m)
        | Broke(m)
        | BadRequest(m)
        | Forbidden(m)
        | NotFound(m)
        | Downtime(m)
        | Overloaded(m)
        | BadCredentials(m)
          -> Some(m)
    in
    match err_str with 
      | None -> ()
      | Some(m) -> error_msg m
  in
  let resp = poll_from_twitter() in
  match resp with 
    | Error(m) -> report_error m
    | Tweets(m) -> 
      let per_tweet stat = normal_msg (stat.user.handle ^ ": " ^ stat.text) in 
      List.iter per_tweet m

let sighand, user_loop = 
  let intr = (ref false) in
  begin 
    Signal_handle(fun (signum:int) -> 
      match !intr with
        | true -> print_newline(); exit 0
        | false -> intr := true
  )
  end,
  begin fun () -> 
    if !intr then
      let input = prompt_for_input() in
      intr := false;
      move_cursor 0 (-1);
      erase Below;
      scroll (-1);
      send_to_twitter input
    else
      update_tweets ()
  end

let intr = ref false

let () =
  set_twit_auth "RobertFischer" "Cowpie68";
  let rec loop () = user_loop(); Unix.sleep 2; loop() in
  set_signal sigint sighand;
  erase Screen;
  admin_msg "Welcome to TwitTerm!";
  loop()
