open XmlUtil

let handle =
  Curl.global_init Curl.CURLINIT_GLOBALALL; 
  let h = Curl.init() in
  at_exit (fun () -> Curl.cleanup h; Curl.global_cleanup ());
  h

type error = 
  | NotModified
  | BadRequest of string (* Exceeded Rate Limit *)
  | BadCredentials of string
  | Forbidden of string
  | NotFound of string
  | Broke of string
  | Downtime of string
  | Overloaded of string
  | Unknown of int * string

let error_of_int_str i msg =
  match i with
    | 304 -> NotModified
    | 400 -> BadRequest(msg)
    | 401 -> BadCredentials(msg)
    | 403 -> Forbidden(msg)
    | 404 -> NotFound(msg)
    | 500 -> Broke(msg)
    | 502 -> Downtime(msg)
    | 503 -> Overloaded(msg)
    | _ -> Unknown(i,msg)

type twit_user = {
  number:string;
  name:string;
  handle:string
}

type tweet = {
  text:string;
  id:string;
  user:twit_user
}

type twit_response = Tweets of tweet list | Error of error

let set_twit_auth user pass = Curl.set_userpwd handle (user ^ ":" ^ pass)

let parse_tweets xdata =
  let parse_user xdata = {
    number = xml_child_pcdata "id" xdata "[[NO ID]]";
    name = xml_child_pcdata "name" xdata "[[UNKNOWN]]";
    handle = xml_child_pcdata "screen_name" xdata ""
  } 
  in
  let parse_maybe_user xdata = 
    match xdata with
      | Some(x) -> parse_user x
      | None -> {number="[[NO ID]]"; name="[[UNKNOWN]]";handle=""}
  in
  let parse_status xdata = {
    text= xml_child_pcdata "text" xdata "[[EMPTY]]";
    id= xml_child_pcdata "id" xdata "[[NO ID]]";
    user= parse_maybe_user (xml_child "user" xdata)
  }
  in
  xml_child_map "status" xdata parse_status

let parse_error code xdata def = 
  let body = xml_child_pcdata "error" xdata "" in
  let msg = if body = "" then def else body in
  error_of_int_str code msg

let poll_from_twitter () =
  let code_from h =
    let res = Curl.getinfo h Curl.CURLINFO_RESPONSE_CODE in
    match res with
      | Curl.CURLINFO_Long(i) -> i
      | _                     -> failwith "Expected long value for HTTP code"
  in
  Curl.set_verbose handle true;
  let feed_url = "http://twitter.com/statuses/friends_timeline.xml" in
  Curl.set_url handle feed_url;
  Curl.set_httpauth handle [Curl.CURLAUTH_ANY]; 
  let body = ref "" in
  let write_to_body str = body := (!body ^ str) in
  Curl.set_writefunction handle write_to_body;
  Curl.perform handle;
  let body = Xml.parse_string !body in
  let code = code_from handle in
  match code with
  | 200 -> Tweets(parse_tweets body)
  | 304 -> Tweets([])
  | _ -> Error(parse_error code body "[[UNKNOWN ERROR]]")

let () = let version = Curl.version() in print_endline ("Using curlib version " ^ version)

