let plugin_name = "conc2seq"
let plugin_shortname = "c2s"
let emitter = Emitter.create "C2S" [Emitter.Code_annot] [] []

include Plugin.Register(
  struct
    let name = plugin_name
    let shortname = plugin_shortname
    let help = "Allow to transform a concurrent C API into a sequential \
                code simulating it."
  end)

module Enabled =
  False(
  struct
    let option_name = "-" ^ plugin_shortname
    let help = "when on (off by default), creates a simulation for \
                parallel executions"
  end)

module Check =
  False(
  struct
    let option_name = "-" ^ plugin_shortname ^ "-check"
    let help = "when on (off by default), checks the generated AST \
               after both 'single load' transformation and simulation"
  end)                                          

module OutputFile =
  Empty_string(
  struct
    let option_name = "-" ^ plugin_shortname ^ "-output"
    let arg_name = "output-file"
    let help = "file where the simulation is output, by default it is \
               not output."
  end)
