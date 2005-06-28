# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------
include ../common.mk

EBIN = ../ebin
INCLUDE = ../include
DOCDIR = ../doc

EDOC_OPTS = [{title,"Open SMPP Erlang Library"}]

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
EDOC_MODULES = \
	base_syntax \
	param_syntax \
	pdu_syntax \
	operation \
	sm \
	smpp_log \
	smpp_error \
	gen_esme_session \
	gen_smsc_session \
	gen_esme \
	gen_smsc

MODULES = $(EDOC_MODULES)

EXTERNAL_HEADERS = oserl

INTERNAL_HEADERS = \
	e164 \
	base_syntax \
	param_syntax \
	pdu_syntax \
	smpp_globals \
	smpp_base \
	smpp_param \
	smpp_pdu

ERL_FILES = $(MODULES:%=%.erl)
HRL_FILES = $(EXTERNAL_HEADERS:%=$(INCLUDE)/%.hrl) $(INTERNAL_HEADERS:%=$(INCLUDE)/%.hrl)

DOC_TARGET_FILES = $(EDOC_MODULES:%=$(DOCDIR)/%.html)
TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR))


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -W
ERL_COMPILE_FLAGS += $(DEBUG) -I $(INCLUDE) -pz $(EBIN)


# ----------------------------------------------------
# Targets
# ----------------------------------------------------
all:	$(TARGET_FILES)

doc:	
	$(ERL) -noshell $(EDOC_PATHS) -run edoc_run application \
		"'$(APPNAME)'" '".."' '$(EDOC_OPTS)' -s erlang halt

opt build: $(TARGET_FILES) 

clean:
	$(RM) $(TARGET_FILES)

realclean: clean
	$(RM) $(DOC_TARGET_FILES)
	$(RM) $(DOCDIR)/edoc-info
	$(RM) $(DOCDIR)/index.html
	$(RM) $(DOCDIR)/modules-frame.html
	$(RM) $(DOCDIR)/overview-summary.html
	$(RM) $(DOCDIR)/packages-frame.html
	$(RM) $(DOCDIR)/stylesheet.css


# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
$(EBIN)/%.beam:	%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o $(EBIN) $<


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
RELSYSDIR = $(RELEASE_PATH)/lib/$(APPNAME)-$(VSN)

release_src:
	$(INSTALL_DATA) $(ERL_FILES) $(RELSYSDIR)/src
	$(INSTALL_DATA) $(HRL_FILES) $(RELSYSDIR)/include
	$(INSTALL_DATA) Makefile $(RELSYSDIR)/src

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/ebin
	$(INSTALL_DATA) $(TARGET_FILES) $(RELSYSDIR)/ebin


# ----------------------------------------------------
# .hrl dependencies
# ----------------------------------------------------
$(EBIN)/base_syntax.beam:../include/smpp_globals.hrl
$(EBIN)/base_syntax.beam:../include/base_syntax.hrl
$(EBIN)/param_syntax.beam:../include/smpp_globals.hrl
$(EBIN)/param_syntax.beam:../include/param_syntax.hrl
$(EBIN)/pdu_syntax.beam:../include/smpp_globals.hrl
$(EBIN)/pdu_syntax.beam:../include/pdu_syntax.hrl
$(EBIN)/operation.beam:../include/smpp_globals.hrl
$(EBIN)/operation.beam:../include/smpp_pdu.hrl
$(EBIN)/gen_esme_session.beam:../include/oserl.hrl
$(EBIN)/gen_smsc_session.beam:../include/oserl.hrl
$(EBIN)/gen_esme.beam:../include/oserl.hrl
$(EBIN)/gen_smsc.beam:../include/oserl.hrl
