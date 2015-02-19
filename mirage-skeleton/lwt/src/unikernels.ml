open OS
open Lwt

module Heads1 (C: V1_LWT.CONSOLE) = struct

  let start c =
    bind (join [
        bind (Time.sleep 1.0) (fun () ->
            C.log c "Heads"; return ()
          );
        bind (Time.sleep 2.0) (fun () ->
            C.log c "Tails"; return ()
          );
      ]) (fun () ->
        C.log c ("Finished"); return ()
      )

end

module Heads2 (C: V1_LWT.CONSOLE) = struct

  let start c =
    join [
      (Time.sleep 1.0 >>= fun () -> (C.log c "Heads"; return ()));
      (Time.sleep 2.0 >>= fun () -> (C.log c "Tails"; return ()));
    ] >>= (fun () ->
        C.log c "Finished";
        return ()
      )

end

module Heads3 (C: V1_LWT.CONSOLE) = struct

  let start c =
    let heads =
      Time.sleep 1.0 >>
      return (C.log c "Heads");
    in
    let tails =
      Time.sleep 2.0 >>
      return (C.log c "Tails");
    in
    lwt () = heads <&> tails in
    C.log c "Finished";
    return ()

end

module Timeout1 (C: V1_LWT.CONSOLE) = struct

  let start c =
    Random.self_init ();

    let timeout f t =
      Time.sleep f >>
      match state t with
      | Return v -> return (Some v)
      | _        -> cancel t; return None
    in

    let t = Time.sleep (Random.float 3.0) >> return "Heads" in
    timeout 2.0 t >>= fun v ->
    C.log c (match v with None -> "cancelled" | Some v -> v);
    C.log c "Finished";
    return ()

end

module Timeout2 (C: V1_LWT.CONSOLE) = struct

  let start c  =
    Random.self_init ();
    let timeout f t =
      let tmout = Time.sleep f in
      pick [
        (tmout >>= fun () -> return None);
        (t >>= fun v -> return (Some v));
      ]
    in
    let t = Time.sleep (Random.float 3.0) >> return "Heads" in
    timeout 2.0 t >>= fun v ->
    C.log c (match v with None -> "Cancelled" | Some v -> v);
    C.log c "Finished";
    return ()

end

module Echo_server1 (C: V1_LWT.CONSOLE) = struct

  let start c =
    let read_line () =
      Time.sleep (Random.float 2.5)
      >> return (String.make (Random.int 20) 'a')
    in
    let rec echo_server = function
      | 0 -> return ()
      | n ->
        lwt s = read_line () in
        C.log c s;
        echo_server (n-1)
    in
    echo_server 10

end
