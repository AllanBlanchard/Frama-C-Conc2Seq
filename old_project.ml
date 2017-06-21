let project : Project.t option ref = ref None

let initialize prj =
  match !project with
  | None -> project := Some prj
  | _ -> ()

let get () =
  match !project with
  | None -> assert false
  | Some(p) -> p
