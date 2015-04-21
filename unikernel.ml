open Str
open Lwt
open V1_LWT


let ht = Hashtbl.create 1000;;

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct

  let report_and_close c message =
    C.log_s c message

  let rec respond c flow =
    S.TCPV4.read flow >>= fun result -> (
      match result with
        | `Eof -> report_and_close c "Connection closure initiated."
        | `Error e ->
             report_and_close c (match e with
            | `Timeout -> "Connection timed out; closing.\n"
            | `Refused -> "Connection refused; closing.\n"
            | `Unknown s -> (Printf.sprintf "Connection error: %s\n" s))
        | `Ok buf ->
            let s = (Cstruct.to_string buf)
            in
            let make_struct output =
               let buf = Io_page.(to_cstruct (get 1)) in
               Cstruct.blit_from_string output 0 buf 0 (String.length output);
               Cstruct.set_len buf (String.length output)
            in
            let cmd = String.sub s 8 3 in
            match cmd with
               | "GET" -> let varlength = String.sub s 14 ((String.index_from s 14 '\r') - 14) in
                          let varname = String.sub s ((String.index_from s 14 '\r')+2) (int_of_string varlength) in
                          let resp =
                            try
                              match (Hashtbl.find ht varname) with
                                | data -> (Printf.sprintf "$%d\r\n%s\r\n" (String.length data) data)
                            with
                               Not_found -> (Printf.sprintf "$-1\r\n")
                          in
                          S.TCPV4.write flow (make_struct (Printf.sprintf "%s" resp));
                           >>= (function
                           | `Ok () -> S.TCPV4.close flow
                           | `Eof -> report_and_close c "Connection closure initated."
                           | `Error _ -> report_and_close c "Connection error during writing; closing.")

               | "SET" -> let varlength = String.sub s 14 ((String.index_from s 14 '\r') - 14) in
                          let varlength_int = int_of_string varlength in
                          let varname = String.sub s ((String.index_from s 14 '\r')+2) varlength_int in
                          let datastart = (14 + String.length varlength + 2 + varlength_int + 3) in
                          let datalength = String.sub s datastart ((String.index_from s datastart '\r') - datastart) in
                          let datalength_int = int_of_string datalength in
                          let data = String.sub s ((String.index_from s datastart '\r')+2) datalength_int in
                            Hashtbl.add ht varname data;
                            S.TCPV4.write flow (make_struct (Printf.sprintf "+OK\r\n" ))
                           >>= (function
                           | `Ok () -> S.TCPV4.close flow
                           | `Eof -> report_and_close c "Connection closure initated."
                           | `Error _ -> report_and_close c "Connection error during writing; closing.")

               | _ -> S.TCPV4.write flow (make_struct (Printf.sprintf "Unrecognized command: %s, the whole thing: XXX%sXXX\n" cmd s))
                           >>= (function
                           | `Ok () -> S.TCPV4.close flow
                           | `Eof -> report_and_close c "Connection closure initated."
                           | `Error _ -> report_and_close c "Connection error during writing; closing.")
        )

  let start c s =
    S.listen_tcpv4 s ~port:6379 (respond c);
    S.listen s

end

