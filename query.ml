(**************************************************************************)
(*  This file is part of Conc2Seq plug-in of Frama-C.                     *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
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

let original  = ref None
let o_to_sl   = ref None
let sl_prj    = ref None
(*let sl_to_sim = ref None*)
let sim_prj   = ref None

let prepare o =
  match !original with
  | None -> original := Some o
  | _ -> assert false

let add_sload lo_to_sl lsl_prj =
  match !o_to_sl, !sl_prj with
  | None, None ->
    o_to_sl := Some lo_to_sl ;
    sl_prj  := Some lsl_prj
  | _, _ -> assert false

let add_simulation prj =
  match !sim_prj with
  | None -> sim_prj := Some prj
  | _ -> assert false

let original f x =
  let p = match !original with
  | None -> assert false
  | Some p -> p
  in Project.on p f x

let sload f x =
  let p = match !sl_prj with
  | None -> assert false
  | Some p -> p
  in Project.on p f x

let simulation f x =
  let p = match !sim_prj with
  | None -> assert false
  | Some p -> p
  in Project.on p f x
