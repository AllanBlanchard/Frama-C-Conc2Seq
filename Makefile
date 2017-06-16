FRAMAC_SHARE := $(shell frama-c -print-path)
FRAMAC_LIBDIR := $(shell frama-c -print-libpath)
PLUGIN_NAME = Conc2Seq
PLUGIN_CMO = \
	errors\
	options\
	old_project\
	atomic_spec\
	atomic_header\
	thread_local\
	single_load\
	vars\
	functions\
	code_transformer\
	statements\
	interleavings\
	simulation\
	core\
	register
#PLUGIN_GUI_CMO = cfg_gui
#PLUGIN_DEPENDENCIES+=Genassigns
include $(FRAMAC_SHARE)/Makefile.dynamic
