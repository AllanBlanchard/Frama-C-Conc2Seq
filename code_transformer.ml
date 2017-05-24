open Cil_types

let partial_visitor = ref None
                          
class expr_visitor prj th loc = object(_)
  inherit Visitor.frama_c_copy prj

  method! vlval _ = 
    let modify (host, offset) =
      match host with
      | Var(vi) -> Vars.access vi.vid ~th:(Some th) ~no:offset loc
      | _       -> host, offset
    in
    Cil.DoChildrenPost modify
end


let old_project_is prj =
  match !partial_visitor with
  | Some(_) ->
     assert false
  | None ->
     partial_visitor := Some (new expr_visitor prj)

let visitor th loc =
  match !partial_visitor with
  | None ->
     assert false
  | Some v -> v th loc
