# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------
include common.mk

SUB_DIRECTORIES = src doc/examples  

OTHER_FILES = COPYING Makefile README TODO changes.txt common.mk $(APPNAME).pub

BASE_REL = /var/tmp/$(APPNAME)-$(VSN)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

TARGETS= all docs clean realclean

#These targets are not real files
.PHONY: $(TARGETS) $(foreach target,$(TARGETS),$(addsuffix /$(target),$(SUB_DIRECTORIES))))

#For each target enter into subdir and make target
define target_template
$1: $(addsuffix /$1,$(SUB_DIRECTORIES));
endef

#For each subdir, make target
define sub_target_template
$(addsuffix /$1,$(SUB_DIRECTORIES)):
	$(MAKE) -C $$(@D) $$(@F)
endef

#evaluate templates into targets
$(foreach target,$(TARGETS),$(eval $(call target_template,$(target))))
$(foreach target,$(TARGETS),$(eval $(call sub_target_template,$(target))))

# 'debug' is the same as 'all' except for the compiler flags
debug: ERL_COMPILE_FLAGS+=-Ddebug=1

debug: all

# Communicate to sub-make
export ERL_COMPILE_FLAGS

release:
	mkdir $(BASE_REL)
	cp -p $(OTHER_FILES) $(BASE_REL)
	mkdir $(BASE_REL)/ebin
	mkdir $(BASE_REL)/doc
	cp -p doc/*.html $(BASE_REL)/doc
	cp -p doc/stylesheet.css $(BASE_REL)/doc
	cp -p doc/overview.edoc $(BASE_REL)/doc
	cp -p doc/oserl_pics.sxw $(BASE_REL)/doc
	mkdir $(BASE_REL)/doc/img
	cp -p doc/img/*.png $(BASE_REL)/doc/img/
	mkdir $(BASE_REL)/doc/examples
	cp -p doc/examples/*.erl $(BASE_REL)/doc/examples/
	cp -p doc/examples/send_oserl.html $(BASE_REL)/doc/examples/
	cp -p doc/examples/Makefile $(BASE_REL)/doc/examples/
	mkdir $(BASE_REL)/priv
	mkdir $(BASE_REL)/include
	mkdir $(BASE_REL)/src
	cd src; make RELSYSDIR=$(BASE_REL) release_src
	cd $(BASE_REL)/..; tar -czvf $(APPNAME)-$(VSN).tar.gz $(APPNAME)-$(VSN)
	mv $(BASE_REL)/../$(APPNAME)-$(VSN).tar.gz .
	rm -rf $(BASE_REL)