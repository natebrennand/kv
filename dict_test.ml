open Dictionary
open Dict

let test_suite ht t =
    let add k v = set ht k v in
    let get k = get ht k in

    let key1 = "a" in

    let () = add key1 3 in
    let () = add "b" 3 in
    let () = assert (get key1 = 3) in
    Format.printf "tests passed for %s\n" t


let () =
    let ht = Dict.create 50 in
    let () = test_suite ht "Hashtbl wrapper" in
    ()

