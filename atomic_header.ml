let some_thread () =
  try Globals.Functions.find_by_name "some_thread" with
  | Not_found -> raise (Errors.MissingAtomicFile "some_thread")
