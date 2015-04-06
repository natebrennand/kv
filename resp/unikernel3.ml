open Str
open Lwt
open V1_LWT
open Dictionary
open Dict

let red fmt    = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = Printf.sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = Printf.sprintf ("\027[36m"^^fmt^^"\027[m")
let ht = Dict.create 50

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct
  let report_and_close c flow message =
    C.log c message;
    S.TCPV4.close flow

  let rec respond c flow =
    S.TCPV4.read flow >>= fun result -> (
      match result with
        | `Eof -> report_and_close c flow "Connection closure initiated."
        | `Error e ->
          let message =
          match e with
            | `Timeout -> "Connection timed out; closing.\n"
            | `Refused -> "Connection refused; closing.\n"
            | `Unknown s -> (Printf.sprintf "Connection error: %s\n" s)
             in
          report_and_close c flow message
        | `Ok buf ->
            let s = (Cstruct.to_string buf)
            in
            let make_struct output =
               let buf = Io_page.(to_cstruct (get 1)) in
               Cstruct.blit_from_string output 0 buf 0 (String.length output);
               Cstruct.set_len buf (String.length output)
            in
            let cmd = String.sub s 0 3
            in
            match cmd with
               | "get" -> let varname = String.sub s 4 (String.length s - 4) in
                          let resp = match (get ht varname) with
                             | data -> (Printf.sprintf "GET variable %s \n" varname)
                             | _ -> (Printf.sprintf "ERROR GET variable %s \n" varname)
                          in
                          S.TCPV4.write flow (make_struct resp)
                                       >>= (function
                                            | `Ok () -> respond c flow
                                            | `Eof -> report_and_close c flow "Connection closure initated."
                                            | `Error _ -> report_and_close c flow "Connection error during writing; closing.")
               | "set" -> let varname = String.sub s 4 ((String.rindex s ' ') - 4) in
                          let value = String.sub s ((String.rindex s ' ') + 1) (String.length s - 1 - (String.rindex s ' ')) in
                          S.TCPV4.write flow (make_struct (Printf.sprintf "SET variable %s to value %s\n" varname value))
                           >>= (function
                           | `Ok () -> respond c flow
                           | `Eof -> report_and_close c flow "Connection closure initated."
                           | `Error _ -> report_and_close c flow "Connection error during writing; closing.")
               | _ -> S.TCPV4.write flow (make_struct (Printf.sprintf "Unrecognized command: %s\n" cmd))
                           >>= (function
                           | `Ok () -> respond c flow
                           | `Eof -> report_and_close c flow "Connection closure initated."
                           | `Error _ -> report_and_close c flow "Connection error during writing; closing.")
        )

  let start c s = 
    S.listen_tcpv4 s ~port:6379 (respond c);
    S.listen s

end

