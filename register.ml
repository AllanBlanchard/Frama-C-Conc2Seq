let run () =
  let prj = Single_load.make "Single memory loads" in
  if Options.Check.get() then
    Project.on prj Filecheck.check_ast "Checking" ;
  ()

let () = 
  Db.Main.extend run
