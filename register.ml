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

let contains s1 s2 =
  let re = Str.regexp_string s2
  in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false

let run () =
  let enabled = Options.Enabled.get() in
  
  try
    if enabled then
      let check   = Options.Check.get() in
      
      Query.prepare (Project.current()) ;
      let extra_args = Dynamic.Parameter.String.get "-cpp-extra-args" () in
      if not (contains extra_args "-CC") then begin
        Options.warning "We require the compiler to keep comments during \
                         MACRO expansion, allowing to generate specification \
                         for functions in atomic.h.\nAdding -CC to \
                         -cpp-extra-args" ;
        Dynamic.Parameter.String.set "-cpp-extra-args" (extra_args ^ " -CC")
      end ;
      let sl_prj, orig_to_sl = Single_load.make "Single memory loads" in
      Query.add_sload orig_to_sl sl_prj ;
    
      if check then
        Query.sload Filecheck.check_ast "Checking single load AST" ;

      ignore(Simulation.make ()) ;

      if check then
        Query.simulation Filecheck.check_ast "Checking simulation AST" ;

      let filename = Options.OutputFile.get() in
      if not(0 = String.compare filename "") then begin
        if not (0 = String.compare filename "stdout") then begin
          Query.simulation (fun () ->
              let out_file  = open_out filename in
              let formatter = Format.formatter_of_out_channel out_file in
              File.pretty_ast ~prj:(Project.current()) ~fmt:formatter ();
              close_out out_file
            ) ()
        end
        else
          Query.simulation (fun () ->
              let ast = Ast.get() in
              Options.feedback "%a" Printer.pp_file ast
            ) ()
      end;
    else
      ()
  with
  | Errors.BadConstruct(s) ->
    Options.abort "%s are not supported" s
  | Errors.MissingAtomicFile(s) ->
    Options.abort "%s not found, atomic.h not included ?" s 

let () = 
  Db.Main.extend run
