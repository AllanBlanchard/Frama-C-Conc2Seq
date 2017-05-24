let run () =
  let sl_prj = Single_load.make "Single memory loads" in
  if Options.Check.get() then
    Project.on sl_prj Filecheck.check_ast "Checking" ;
  let ast = Project.on sl_prj Ast.get() in
  let sim_prj = Project.on sl_prj Simulation.make ast in
  if Options.Check.get() then
    Project.on sim_prj Filecheck.check_ast "Checking" ;
  ()

let () = 
  Db.Main.extend run
