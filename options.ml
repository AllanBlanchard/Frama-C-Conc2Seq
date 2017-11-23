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

let plugin_name = "conc2seq"
let plugin_shortname = "c2s"
let emitter = Emitter.create "C2S" [Emitter.Code_annot] [] []

include Plugin.Register(
  struct
    let name = plugin_name
    let shortname = plugin_shortname
    let help = "Allow to transform a concurrent C API into a sequential \
                code simulating it."
  end)

module Enabled =
  False(
  struct
    let option_name = "-" ^ plugin_shortname
    let help = "when on (off by default), creates a simulation for \
                parallel executions"
  end)

module Check =
  False(
  struct
    let option_name = "-" ^ plugin_shortname ^ "-check"
    let help = "when on (off by default), checks the generated AST \
               after both 'single load' transformation and simulation"
  end)                                          

module OutputFile =
  Empty_string(
  struct
    let option_name = "-" ^ plugin_shortname ^ "-output"
    let arg_name = "output-file"
    let help = "file where the simulation is output, by default it is \
               not output."
  end)
