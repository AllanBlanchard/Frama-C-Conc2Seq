FRAMAC_SHARE := $(shell frama-c -print-path)
FRAMAC_LIBDIR := $(shell frama-c -print-libpath)
PLUGIN_NAME = Conc2Seq
PLUGIN_CMO = \
	errors\
	options\
	query\
	atomic_spec\
	atomic_header\
	thread_local\
	simulation_invariant\
	single_load\
	vars\
	vars_spec\
	functions\
	simulation_axioms\
	code_transformer\
	statements\
	interleavings\
	simfuncs_spec\
	logic_transformer\
	simulation\
	register
#PLUGIN_GUI_CMO = cfg_gui
#PLUGIN_DEPENDENCIES+=Genassigns
include $(FRAMAC_SHARE)/Makefile.dynamic
