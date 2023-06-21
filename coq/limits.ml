(* We'd like to move this code to Lang, but it is still too specific *)

module Token : sig
  type t

  val make : unit -> t
  val set : t -> unit
  val is_set : t -> bool
end = struct
  type t = bool ref

  let make () = ref false
  let set t =
    t := true;
    Control.interrupt := true
  let is_set t = !t
end

let limit ~token ~f x =
  if Token.is_set token then
    Error Sys.Break
  else
    let () = Control.interrupt := false in
    try
      Ok (f x)
    with Sys.Break ->
      Error Sys.Break
