# FIXME These variables should be automatically set when compiling
ERL = erl
ERLC = erlc
ETAGS = etags
EMULATOR = beam
EBIN = ../ebin
INCLUDE = ../include
ESRC = ./
DOCDIR = ../doc
RM = rm -f
INSTALL = /usr/bin/install -c
INSTALL_DIR = /usr/bin/install -c -d
INSTALL_DATA = ${INSTALL} -m 644


### oserl use eDoc for documentation, to regenerate update paths as needed!
OSERL_APP = ..
EDOC_APP = /usr/local/lib/erlang/lib/edoc-0.4
SYNTAX_TOOLS_APP = /usr/local/lib/erlang/lib/syntax_tools-1.3



# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
include ../vsn.mk
VSN=$(OSERL_VSN)
APPNAME = oserl

RELSYSDIR = $(RELEASE_PATH)/lib/$(APPNAME)-$(VSN)


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------

EDOC_MODULES = \
	base_syntax \
	param_syntax \
	pdu_syntax \
	operation \
	sm \
	gen_connection \
	gen_esme_session \
	gen_esme        

MODULES = $(EDOC_MODULES)


EXTERNAL_HEADERS = gen_esme

INTERNAL_HEADERS = \
	e164 \
	base_syntax \
	param_syntax \
	pdu_syntax \
	smpp_globals \
	smpp_base \
	smpp_param \
	smpp_pdu \
	gen_esme_session


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

doc:	$(DOC_TARGET_FILES)

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
EDOC_PATHS = \
	-pa $(EDOC_APP)/ebin -pa $(OSERL_APP)/ebin -pa $(SYNTAX_TOOLS_APP)/ebin


$(EBIN)/%.beam:	%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o $(EBIN) $<



$(DOCDIR)/%.html:	%.erl
	$(ERL) $(EDOC_PATHS) -s edoc file $< ['{dir,"$(DOCDIR)"}'] -s erlang halt


# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
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
$(EBIN)/operation.beam:../include/base_syntax.hrl
$(EBIN)/operation.beam:../include/param_syntax.hrl
$(EBIN)/operation.beam:../include/pdu_syntax.hrl
$(EBIN)/operation.beam:../include/smpp_base.hrl
$(EBIN)/operation.beam:../include/smpp_param.hrl
$(EBIN)/operation.beam:../include/smpp_pdu.hrl
$(EBIN)/gen_esme_session.beam:../include/smpp_globals.hrl
$(EBIN)/gen_esme_session.beam:../include/gen_esme_session.hrl
$(EBIN)/gen_esme.beam:../include/smpp_globals.hrl
$(EBIN)/gen_esme.beam:../include/smpp_base.hrl
$(EBIN)/gen_esme.beam:../include/gen_esme.hrl
