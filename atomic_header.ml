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
