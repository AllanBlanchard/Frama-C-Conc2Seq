open Cil_types

let has_thread_local_attribute attr = 
  match attr with Attr("thread_local",[]) -> true | _ -> false

let is_thread_local v = 
  List.exists has_thread_local_attribute v.vattr
