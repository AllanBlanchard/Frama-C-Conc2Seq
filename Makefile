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
	single_load\
	vars\
	functions\
	code_transformer\
	statements\
	interleavings\
	program_counter\
	simulation_invariant\
	vars_spec\
	simulation_axioms\
	fun_preds\
	user_invariant\
	simfuncs_spec\
	lemmas\
	simulation\
	register
#PLUGIN_GUI_CMO = cfg_gui
#PLUGIN_DEPENDENCIES+=Genassigns
include $(FRAMAC_SHARE)/Makefile.dynamic
