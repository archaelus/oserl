# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------
include ../../common.mk

EBIN = .
EBIN_OSERL = ../../ebin
INCLUDE = ../../include
DOCDIR = ..

EDOC_OPTS = [{dir, "$(DOCDIR)"}]


# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
EDOC_MODULES = echo_esme code_lock_esme submit_esme

MODULES = $(EDOC_MODULES)

ERL_FILES = $(MODULES:%=%.erl)

DOC_TARGET_FILES = $(EDOC_MODULES:%=$(DOCDIR)/%.html)
TARGET_FILES = $(MODULES:%=$(EBIN)/%.$(EMULATOR))


# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -W
ERL_COMPILE_FLAGS += $(DEBUG) -I $(INCLUDE) -pz $(EBIN_OSERL)


# ----------------------------------------------------
# Targets
# ----------------------------------------------------
all:	$(TARGET_FILES)

doc:	$(DOC_TARGET_FILES)

clean:
	$(RM) $(TARGET_FILES)

realclean: clean
	$(RM) $(DOC_TARGET_FILES)


# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------
$(EBIN)/%.beam:	%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o $(EBIN) $<

$(DOCDIR)/%.html:	%.erl
	$(ERL) -noshell $(EDOC_PATHS) -run edoc_run file "'$<'" '$(EDOC_OPTS)' \
		-s erlang halt


# ----------------------------------------------------
# .hrl dependencies
# ----------------------------------------------------
$(EBIN)/code_lock_esme.beam:../../include/oserl.hrl
$(EBIN)/echo_esme.beam:../../include/oserl.hrl
$(EBIN)/submit_esme.beam:../../include/oserl.hrl
