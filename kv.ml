open Lwt
open V1_LWT

exception Invalid_argument of string

type hash_values =
  | Int of int
  | Str of string
  | ValList of hash_values list

let hashtable = Hashtbl.create 1000;;


type req_data =
  | Number of int
  | String of string
  | Error of string
  | Array of req_data list
  | PING


type response =
  | PONG
  | OK
  | NIL
  | Values of hash_values
  | ERROR of string


(* value of number from ascii char *)
let num_val chr =
  Char.code chr - Char.code '0'


(* parse an integer
 * Returns an integer and the shifted buffer
 * iterate until a '\r' is found, follows:
 * http://redis.io/topics/protocol#high-performance-parser-for-the-redis-protocol
 *
 * returns (n, buf)
 * *)
let parse_num buf =
  let rec find_len buf n =
    let c = Cstruct.get_char buf 0 in
    if '\r' = c then (Cstruct.shift buf 2), n
    else find_len (Cstruct.shift buf 1) (n*10 + num_val c)
  in find_len buf 0


(* parse a bulk string
 * - A "$" byte followed by the number of bytes composing the string (a prefixed length), terminated by CRLF.
 * - The actual string data.
 * - A final CRLF.
 * *)
let parse_bulk_str buf len =
  let s = Cstruct.copy buf 0 len in
  (Cstruct.shift buf (len+2)), s


(* get_resp_len parses the first line of a RESP request
 * returns (lines in request, offset from first line) *)
let get_resp_len buf =
  let astr = Cstruct.get_char buf 0 in
  if astr = '*'
    then parse_num (Cstruct.shift buf 1)
    else buf, 0


(* parse_request takes a cstruct and parses a
 *   command. *)
let parse_request buf =
  let rec parse_array buf len =
    if len = 0
      then buf, []
      else
        let buf, arg = parse_arg buf in
        let buf, args = parse_array buf (len - 1) in
        buf, arg :: args
  and parse_arg buf = match Cstruct.get_char buf 0 with
    | '*' ->
      let buf, num_args = parse_num (Cstruct.shift buf 1) in
      let buf, args = parse_array buf num_args in
      buf, Array(args)
    | '$' ->
      let buf, len = parse_num (Cstruct.shift buf 1) in
      let buf, str = parse_bulk_str buf len in
      buf, String(str)
    | 'p' | 'P' ->
        if "PING\r\n" = String.uppercase(Cstruct.copy buf 0 6)
          then Cstruct.shift buf 6, PING
          else buf, Error "ERR: inline text started with 'p' and was not PING"
    | _ ->
      buf, Error("ERR: unrecognized command")
  in
  parse_arg buf


(* handle_get finds the requested value in the hashtable *)
let rec handle_get = function
  | String(key) :: [] ->
    if Hashtbl.mem hashtable key
      then Values(Hashtbl.find hashtable key)
      else NIL
  | _ -> ERROR("ERR: only string keys can be used with GET")


(* handle_set sets the specified key/value pair in the hashtable *)
let rec handle_set = function
  | String(key) :: String(str) :: [] ->
    let () = Hashtbl.replace hashtable key (Str str) in
    OK
  | String(key) :: Number(n) :: [] ->
    let () = Hashtbl.replace hashtable key (Int n) in
    OK
  | _ -> ERROR("ERR: invalid SET arguments")


(* handle_request forms a resp object based on the request *)
let handle_request buf =
  let form_request = function
    | String(g) :: a
      when String.uppercase g = "GET" -> handle_get a
    | String(s) :: a
      when String.uppercase s = "SET" -> handle_set a
    | String(p) :: a
      when String.uppercase p = "PING" -> PONG
    | _ -> ERROR("ERR: unsupported command")
  in
  let buf, args = parse_request buf in
  match args with
    | Array(a) -> form_request a
    | PING     -> PONG
    | _        -> ERROR("ERR: unrecognized commands")


(* write_response forms a string based on the resp object's
 *   values. *)
let write_response r =
  let error_str e  = Printf.sprintf "-%s\r\n" e in
  let simple_str s = Printf.sprintf "+%s\r\n" s in
  let simple_num i = Printf.sprintf ":%d\r\n" i in
  let complex_val v =
    let l = 1 in
    let v = match v with
      | Int i -> Printf.sprintf ":%d\r\n" i
      | Str s -> Printf.sprintf "$%d\r\n%s\r\n" (String.length s) s
      | ValList l -> raise (Invalid_argument "List cannot be used in complex_val")
    in Printf.sprintf "*%d\r\n%s" l v
  in
  let list_vals l =
    let len = List.length l in
     Printf.sprintf "*%s\r\n%s"
       (string_of_int len)
       (String.concat "" (List.map complex_val l))
  in
  match r with
  | PONG              -> simple_str "PONG"
  | OK                -> simple_str "OK"
  | NIL               -> "$-1\r\n"
  | ERROR e           -> error_str e
  | Values(Int i)     -> simple_num i
  | Values(Str s)     -> simple_str s
  | Values(ValList l) -> list_vals l
  | _ -> "-ERR: not implemented\r\n"


module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct

  (* Logs an error then closes the conn *)
  let report_and_close c flow message =
    C.log c message;
    S.TCPV4.close flow


  (* Writes an error message to the flow *)
  let send_err flow message =
    let _ = S.TCPV4.write flow message in ()


  let string_to_cstruct str =
    let l = String.length str in
    let msg = Cstruct.create l in
    let () = Cstruct.blit_from_string str 0 msg 0 l in
    msg

  let handle_err_read c flow e =
    let message = match e with
      | `Timeout -> "Echo connection timed out; closing.\n"
      | `Refused -> "Echo connection refused; closing.\n"
      | `Unknown s -> (Printf.sprintf "Echo connection error: %s\n" s)
    in report_and_close c flow message

  let rec handle c flow =
    let _ = C.log_s c "handling client" in
    S.TCPV4.read flow >>= fun result -> (
      match result with
        | `Eof -> report_and_close c flow "Echo connection closure initiated."
        | `Error e -> handle_err_read c flow e
        | `Ok buf ->
            let _ = C.log_s c (Printf.sprintf "REQ: {%s}" (Cstruct.to_string buf)) in
            let msg = handle_request buf |> write_response |> string_to_cstruct in
            let _ = C.log_s c (Printf.sprintf "RESP: {%s}" (Cstruct.to_string msg)) in
            S.TCPV4.write flow msg;
              >>= (function
                | `Ok () -> handle c flow  (* wait for the next command *)
                | `Eof   -> handle c flow  (* wait for the next command *)
                (*
                | `Ok ()   -> report_and_close c flow "request served successfully, closing connection"
                | `Eof     -> report_and_close c flow "EOF, closing connection"
                *)
                | `Error _ -> report_and_close c flow "Connection error during writing; closing.")
        )

  let start c s =
    S.listen_tcpv4 s ~port:6379 (handle c);
    let _ = C.log_s c "listening on port 6379" in
    S.listen s

end
