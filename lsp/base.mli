(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(************************************************************************)
(* Coq Language Server Protocol                                         *)
(* Copyright 2019 MINES ParisTech -- LGPL 2.1+                          *)
(* Copyright 2019-2022 Inria -- LGPL 2.1+                               *)
(* Written by: Emilio J. Gallego Arias                                  *)
(************************************************************************)

module Point : sig
  type t =
    { line : int
    ; character : int
    }
end

module Range : sig
  type t =
    { start : Point.t
    ; _end : Point.t
    }
end

module Diagnostic : sig
  type t =
    { range : Range.t
    ; severity : int
    ; message : string
    }
end

val mk_range : Range.t -> Yojson.Basic.t
val mk_reply : id:int -> result:Yojson.Basic.t -> Yojson.Basic.t

(* val mk_diagnostic : Range.t * int * string * unit option -> Yojson.Basic.t *)
val mk_diagnostics :
     uri:string
  -> version:int
  -> (Range.t * int * string * unit option) list
  -> Yojson.Basic.t

val std_protocol : bool ref
