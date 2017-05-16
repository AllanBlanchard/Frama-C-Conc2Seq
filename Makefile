FRAMAC_SHARE := $(shell frama-c -print-path)
FRAMAC_LIBDIR := $(shell frama-c -print-libpath)
PLUGIN_NAME = Conc2Seq
PLUGIN_CMO = options thread_local single_load core register
#PLUGIN_GUI_CMO = cfg_gui
#PLUGIN_DEPENDENCIES+=Genassigns
include $(FRAMAC_SHARE)/Makefile.dynamic
