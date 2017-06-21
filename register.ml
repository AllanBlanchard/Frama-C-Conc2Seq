let run () =
  Old_project.initialize (Project.current()) ;
  let sl_prj = Single_load.make "Single memory loads" in
  if Options.Check.get() then
    Project.on sl_prj Filecheck.check_ast "Checking" ;

  (*
  let ast = Project.on sl_prj Ast.get() in
  try
    let sim_prj = Project.on sl_prj Simulation.make ast in
    if Options.Check.get() then
      Project.on sim_prj Filecheck.check_ast "Checking"
  with
  | Errors.BadConstruct(s) ->
     Options.Self.error "%s are forbidden" s
  | Errors.MissingAtomicFile(s) ->
     Options.Self.error "%s not found, atomic.h not included ?" s
    ;
   *)
  ()

let () = 
  Db.Main.extend run
