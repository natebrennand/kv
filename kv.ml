open Lwt
open V1_LWT


(*
cstruct req {
} as big_endian
*)

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct
  let report_and_close c flow message =
    C.log c message;
    S.TCPV4.close flow

  let send_err flow message =
    let _ = S.TCPV4.write flow message in
    ()


  (* get_resp_len parses the first line of a RESP request
   * returns (lines in request, offset from first line) *)
  let get_resp_len buf =
    (* iterate until a '\r' is found, follows
     * http://redis.io/topics/protocol#high-performance-parser-for-the-redis-protocol *)
    let rec find_len buf n offset =
      let c = Cstruct.get_char buf offset in
      if '\r' = c then
        n, offset
      else
        let digit = Char.code c - Char.code '0' in
        find_len buf (n*10 + digit) (offset+1)
    in
    let astr = Cstruct.get_char buf 0 in
    if astr = '*' then
      let n, offset = find_len buf 0 1 in
      n, offset+2  (* add 2 for '\r\n' *)
    else
      0, 0



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
            let n, offset = get_resp_len buf in
            let _ = C.log_s c (Printf.sprintf "{%d in %d bytes}" n offset) in
            S.TCPV4.write flow buf
            >>= function _ -> handle c flow
        )

  let start c s =
    S.listen_tcpv4 s ~port:6379 (handle c);
    let _ = C.log_s c "listening on port 6379" in
    S.listen s

end
