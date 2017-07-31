let run () =
  Query.prepare (Project.current()) ;
  
  let sl_prj, orig_to_sl =
      try
        Single_load.make "Single memory loads"
      with
      | Errors.BadConstruct(s) ->
        Options.Self.error "%s are forbidden" s ;
        failwith s
  in
  Query.add_sload orig_to_sl sl_prj ;
  
  if Options.Check.get() then
    Query.sload Filecheck.check_ast "Checking" ;
      
  try
    ignore( Simulation.make () ) ;
    if Options.Check.get() then
      Query.simulation Filecheck.check_ast "Checking" ;
  with
  | Errors.BadConstruct(s) ->
    Options.Self.error "%s are forbidden" s
  | Errors.MissingAtomicFile(s) ->
    Options.Self.error "%s not found, atomic.h not included ?" s
      

let () = 
  Db.Main.extend run
