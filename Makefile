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
all:
	@for d in $(SUB_DIRECTORIES); do \
	  	(cd $$d; $(MAKE)); \
	done

docs:
	@for d in $(SUB_DIRECTORIES); do \
		(cd $$d; $(MAKE) doc); \
	done

clean:
	@for d in $(SUB_DIRECTORIES); do \
	  	(cd $$d; $(MAKE) clean); \
	done

realclean:
	@for d in $(SUB_DIRECTORIES); do \
	  	(cd $$d; $(MAKE) realclean); \
	done

debug:
	@for d in $(SUB_DIRECTORIES); do \
	  	(cd $$d; $(MAKE) DEBUG=-Ddebug=1); \
	done

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


