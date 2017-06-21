let plugin_name = "conc2seq"
let plugin_shortname = "c2s"
let emitter = Emitter.create plugin_shortname [Emitter.Code_annot] [] []

module Self =
  Plugin.Register(
  struct
    let name = plugin_name
    let shortname = plugin_shortname
    let help = "Allow to transform a concurrent C API into a sequential\
                code simulating it."
  end)

module Enabled =
  Self.False(
  struct
    let option_name = "-" ^ plugin_shortname
    let help = "when on (off by default), creates a simulation for\
                parallel executions"
  end)

module Check =
  Self.False(
  struct
    let option_name = "-" ^ plugin_shortname ^ "-check"
    let help = "when on (off by default), checks the generated AST"
  end)                                          

module OutputFile =
  Self.String(
  struct
    let option_name = "-" ^ plugin_shortname ^ "-output"
    let default = "simulation.c"
    let arg_name = "output-file"
    let help = "file where the simulation is output"
  end)
