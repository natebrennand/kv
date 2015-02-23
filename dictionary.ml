
(* String_tbl is a hash table with a string as the key  *)
module String_tbl = Hashtbl.Make(struct
    type t = string
    let equal = (=)
    let hash = Hashtbl.hash
end)


(* Dict_type is the interface for the dictionary holding all data *)
module type Dict_type = sig

    type value =
        | Int of int
        | Float of float
        | String of string

    type tbl = String_tbl of value
    val set : tbl -> string -> value
    val get : (tbl -> string) -> value

end



module Dict  = struct
    let create size = String_tbl.create size
    let set st k v  = String_tbl.replace st k v
    let get st k    = String_tbl.find st k
end

