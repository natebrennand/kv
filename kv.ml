open Lwt
open V1_LWT

exception Invalid_argument of string

type hash_values =
  | Int of int
  | Str of string
  | Nil

let hashtable = Hashtbl.create 1000;;


type req_data =
  | Number of int
  | String of string
  | Array of req_data list
  | PING


type response =
  | PONG
  | OK
  | NIL
  | Values of hash_values
  | ValList of hash_values list
  | ARRAY of response list
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
    if '\r' = c
      then ((Cstruct.shift buf 2), n)
      else find_len (Cstruct.shift buf 1) (n*10 + num_val c)
  in find_len buf 0


(* parse a bulk string
 * - A "$" byte followed by the number of bytes composing the string (a prefixed length), terminated by CRLF.
 * - The actual string data.
 * - A final CRLF.
 * *)
let parse_bulk_str (buf, len) =
  let s = Cstruct.copy buf 0 len in
  (Cstruct.shift buf (len+2)), String(s)


(* get_resp_len parses the first line of a RESP request
 * returns (lines in request, offset from first line) *)
let get_resp_len buf =
  let astr = Cstruct.get_char buf 0 in
  if astr = '*'
    then parse_num (Cstruct.shift buf 1)
    else buf, 0


let parse_ping buf =
  if "PING\r\n" = String.uppercase(Cstruct.copy buf 0 6)
    then Cstruct.shift buf 6, PING
    else raise (Invalid_argument "ERR: inline text started with 'p' and was not PING")


(* parse_request takes a cstruct and parses a
 *   command. *)
let parse_request buf =
  let rec parse_array aggr (buf, len) =
    if len = 0
      then buf, Array(aggr)
      else
        let buf, arg = parse_arg buf in
        parse_array (aggr @ [arg]) (buf, (len -1))
  and parse_arg buf =
      match Cstruct.get_char buf 0 with
      | '*'       -> parse_num (Cstruct.shift buf 1) |> parse_array []
      | '$'       -> parse_num (Cstruct.shift buf 1) |> parse_bulk_str
      | 'p' | 'P' -> parse_ping buf
      | _         -> raise (Invalid_argument "ERR: unrecognized command")
  in
  let rec find_all_args buf aggr =
    let buf, arg = parse_arg buf in
    let args = aggr @ [arg] in
    if Cstruct.len buf > 0
      then find_all_args buf args
      else buf, args
  in
  find_all_args buf []


(* handle_get finds the requested value in the hashtable *)
let handle_get = function
  | String(key) :: [] ->
    if Hashtbl.mem hashtable key
      then Values(Hashtbl.find hashtable key)
      else NIL
  | String(key) :: _ :: [] -> ERROR("ERR: only 1 key can be used with GET")
  | _ -> ERROR("ERR: only string keys can be used with GET")


(* handle_mget finds the requested value(s) in the hashtable *)
let handle_mget args =
  let rec retrieve = function
    | [] -> []
    | String(key) :: rest ->
      (if Hashtbl.mem hashtable key
        then Hashtbl.find hashtable key
        else Nil
      ) :: retrieve rest
    | _ -> raise (Invalid_argument "ERR: only string keys can be used with GET")
  in
  try ValList(retrieve args)
    with Invalid_argument e -> ERROR e


(* handle_set sets the specified key/value pair in the hashtable *)
let handle_set = function
  | String(key) :: String(str) :: [] ->
    Hashtbl.replace hashtable key (Str str); OK
  | String(key) :: Number(n) :: [] ->
    Hashtbl.replace hashtable key (Int n); OK
  | _ -> ERROR("ERR: invalid SET arguments")


(* handle_mset sets the specified key/value pair(s) in the hashtable *)
let handle_mset args =
  let rec establish = function
    | String(key) :: String(str) :: rest ->
      Hashtbl.replace hashtable key (Str str); establish rest
    | String(key) :: Number(n) :: rest ->
      Hashtbl.replace hashtable key (Int n); establish rest
    | [] -> OK
    | _ -> ERROR("ERR: invalid SET arguments")
  in
  if List.length args = 0
    then ERROR("MSET requires > 0 arguments")
    else try establish args
      with Invalid_argument e -> ERROR e


(* handle_request forms a resp object based on the request *)
let handle_request buf =
  let buf, args = parse_request buf in
  let form_request = function
    | String(s) :: a -> (
      match (String.uppercase s) with
        | "GET"   -> handle_get a
        | "MGET"  -> handle_mget a
        | "SET"   -> handle_set a
        | "MSET"  -> handle_mset a
        | "PING"  -> PONG
        | _ -> ERROR("ERR: unsupported command")
      )
    | _ -> ERROR("ERR: unsupported command")
  in
  let rec form_resp = function
    | [] -> []
    | Array(a) :: rest -> (form_request a) :: form_resp rest
    | PING :: rest     -> Values(Str "PONG") :: form_resp rest
    | (Number _ | String _) :: _    -> ERROR("ERR: unrecognized commands") :: []
  in
  try (* catch all possible errors HERE *)
    form_resp args
  with Invalid_argument e -> ((ERROR e) :: [])


(* write_response forms a string based on the resp object's
 *   values. *)
let write_response r =
  let error_str e  = Printf.sprintf "-%s\r\n" e in
  let simple_str s = Printf.sprintf "+%s\r\n" s in
  let simple_num i = Printf.sprintf ":%d\r\n" i in
  let complex_val = function
    | Int i -> Printf.sprintf ":%d\r\n" i
    | Str s -> Printf.sprintf "$%d\r\n%s\r\n" (String.length s) s
    | Nil   -> "$-1\r\n"
  in
  let list_vals l =
    let len = List.length l in
     Printf.sprintf "*%s\r\n%s"
       (string_of_int len)
       (String.concat "" (List.map complex_val l))
  in
  let serialize_resp = function
    (* translates a resp object to a string *)
    | PONG          -> simple_str "PONG"
    | OK            -> simple_str "OK"
    | NIL           -> "$-1\r\n"
    | ERROR e       -> error_str e
    | Values(Int i) -> simple_num i
    | Values(Str s) -> simple_str s
    | ValList l     -> list_vals l
    | _ -> "-ERR: not implemented\r\n"
  in
  (* translate all resp objects to strings and combine them *)
  r |> List.map serialize_resp |> String.concat ""

(*
 * ================================================================================
 * ================================================================================
 * ================================================================================
*)

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct

  (* Logs an error then closes the conn *)
  let report_and_close c flow message =
    C.log c message;
    S.TCPV4.close flow


  (* string_to_cstruct translates a string to a Cstruct.t *)
  let string_to_cstruct str =
    let l = String.length str in
    let msg = Cstruct.create l in
    let () = Cstruct.blit_from_string str 0 msg 0 l in
    msg


  (* handle_err_read translates an io error to a string *)
  let handle_err_read c flow e =
    let message = match e with
      | `Timeout -> "connection timed out; closing.\n"
      | `Refused -> "connection refused; closing.\n"
      | `Unknown s -> (Printf.sprintf "Echo connection error: %s\n" s)
    in report_and_close c flow message


  let rec handle c flow =
    let _ = C.log c "handling client" in
    S.TCPV4.read flow >>= (function
      | `Eof     -> report_and_close c flow "Connection closure initiated."
      | `Error e -> handle_err_read c flow e
      | `Ok buf  ->
        let _ = C.log c (Printf.sprintf "REQ: {%s}"
          (buf |> Cstruct.to_string |> String.trim)) in
        let msg = buf |> handle_request |> write_response |> string_to_cstruct in
        let _ = C.log c (Printf.sprintf "REQ: {%s} RESP: {%s}"
          (buf |> Cstruct.to_string |> String.trim)
          (msg |> Cstruct.to_string |> String.trim))
        in
        S.TCPV4.write flow msg >>= (function
          | `Ok ()   -> let _ = C.log c "OK response" in handle c flow
          | `Eof     -> report_and_close c flow "Connection error during writing; closing."
          | `Error _ -> report_and_close c flow "Connection error during writing; closing."
        )
    )

  let start c s =
    S.listen_tcpv4 s ~port:6379 (handle c);
    let _ = C.log_s c "listening on port 6379" in
    S.listen s

end
