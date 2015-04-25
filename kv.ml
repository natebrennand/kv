open Lwt
open V1_LWT



type resp_data =
  | Number of int
  | String of string
  | Error of string
  | Array of resp_data list
  | PONG


type command =
  | PING
  | GET of string list
  | SET of (string * resp_data) list



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


let parse_ping buf =
  "ING" = Cstruct.copy buf 0 3


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
    | 'P' -> if parse_ping (Cstruct.shift buf 1)
      then buf, PONG
      else buf, Error("ERR: expected 'ping'")
    | '$' ->
      let buf, len = parse_num (Cstruct.shift buf 1) in
      let buf, str = parse_bulk_str buf len in
      buf, String(str)
    | _ ->
      buf, Error("ERR: unrecognized command")
  in
  parse_arg buf



module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct

  (* Logs an error then closes the conn *)
  let report_and_close c flow message =
    C.log c message;
    S.TCPV4.close flow


  (* Writes an error message to the flow *)
  let send_err flow message =
    let _ = S.TCPV4.write flow message in
    ()




  let rec handle c flow =
    let _ = C.log_s c "handling client" in
    S.TCPV4.read flow >>= fun result -> (
      match result with
        | `Eof -> report_and_close c flow "Echo connection closure initiated."
        | `Error e ->
          let message = match e with
            | `Timeout -> "Echo connection timed out; closing.\n"
            | `Refused -> "Echo connection refused; closing.\n"
            | `Unknown s -> (Printf.sprintf "Echo connection error: %s\n" s)
          in report_and_close c flow message
        | `Ok buf ->
            let _ = C.log_s c (Printf.sprintf "{%s}" (Cstruct.to_string buf)) in
            let n, buf = get_resp_len buf in
            let _ = C.log_s c (Printf.sprintf "{%d args, left:{%s}}" n (Cstruct.to_string buf)) in
            S.TCPV4.write flow buf
            >>= function _ -> handle c flow
        )

  let start c s =
    S.listen_tcpv4 s ~port:6379 (handle c);
    let _ = C.log_s c "listening on port 6379" in
    S.listen s

end
