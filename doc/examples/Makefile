ERLC = erlc
EMULATOR = beam
EBIN = .
RM = rm -f

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------
include ../../vsn.mk
VSN = $(OSERL_VSN)


MODULES = echo_esme code_lock_esme


ERL_FLAGS += -W
ERL_COMPILE_FLAGS += $(DEBUG) -I ../../include

SUB_DIRECTORIES = 

all:   $(MODULES:%=$(EBIN)/%.$(EMULATOR))

clean:
	@for d in $(SUB_DIRECTORIES); do \
	  	cd $$d; $(MAKE) clean; \
	done
	$(RM) $(EBIN)/*.beam

realclean: clean

$(EBIN)/%.beam:	%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o $(EBIN) $<

