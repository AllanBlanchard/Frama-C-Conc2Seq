let contains s1 s2 =
  let re = Str.regexp_string s2
  in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false

let run () =
  Query.prepare (Project.current()) ;
  let extra_args = Dynamic.Parameter.String.get "-cpp-extra-args" () in
  if not (contains extra_args "-CC") then begin
    Options.Self.warning "We require the compiler to keep comments during \
                          MACRO expansion, allowing to generate specification \
                          for functions in atomic.h.\nAdding -CC to \
                          -cpp-extra-args" ;
    Dynamic.Parameter.String.set "-cpp-extra-args" (extra_args ^ " -CC")
  end ;
  try
    if Options.Enabled.get() then
      let sl_prj, orig_to_sl = Single_load.make "Single memory loads" in
      Query.add_sload orig_to_sl sl_prj ;
    
      if Options.Check.get() then
        Query.sload Filecheck.check_ast "Checking single load AST" ;

      ignore(Simulation.make ()) ;

      if Options.Check.get() then
        Query.simulation Filecheck.check_ast "Checking simulation AST" ;

      let filename = Options.OutputFile.get() in
      if not(0 = String.compare filename "") then begin
        if not (0 = String.compare filename "stdout") then begin
          Query.simulation (fun () ->
              let out_file  = open_out (Options.OutputFile.get()) in
              let formatter = Format.formatter_of_out_channel out_file in
              File.pretty_ast ~prj:(Project.current()) ~fmt:formatter ();
              close_out out_file
            ) ()
        end
        else
          Query.simulation (fun () ->
              let ast = Ast.get() in
              Options.Self.feedback "%a" Printer.pp_file ast
            ) ()
      end;
    else
      ()
  with
  | Errors.BadConstruct(s) ->
    Options.Self.abort "%s are not supported" s
  | Errors.MissingAtomicFile(s) ->
    Options.Self.abort "%s not found, atomic.h not included ?" s 

let () = 
  Db.Main.extend run
