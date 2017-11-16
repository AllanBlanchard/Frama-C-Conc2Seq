(**************************************************************************)
(*  This file is part of Conc2Seq plug-in of Frama-C.                     *)
(*                                                                        *)
(*  Copyright (C) 2016-2017 Allan Blanchard                               *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 3.                                                *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 3                   *)
(*  for more details (enclosed in the file LICENCE).                      *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

let some_thread () =
  try Globals.Functions.find_by_name "some_thread" with
  | Not_found -> raise (Errors.MissingAtomicFile "some_thread")

let valid_thread_id t =
  let f = match Logic_env.find_all_logic_functions "valid_thread_id" with
    | [f] -> f
    | _ -> raise (Errors.MissingAtomicFile "valid_thread_id")
  in
  Logic_const.papp (f, [], [t])


let max_thread () =
  match Logic_env.find_all_logic_functions "MAX_THREAD" with
  | [f] -> Logic_const.term (Tapp(f, [], [])) Linteger 
  | _ -> raise (Errors.MissingAtomicFile "MAX_THREAD")
