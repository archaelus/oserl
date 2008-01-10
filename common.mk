# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------
APPNAME = oserl
VSN = 1.3

# FIXME These variables should be automatically set when compiling
ERL = erl
ERLC = erlc +debug_info
#ETAGS = etags
EMULATOR = beam
RM = rm -f
INSTALL = /usr/bin/install -c
INSTALL_DIR = /usr/bin/install -c -d
INSTALL_DATA = ${INSTALL} -m 644

# ----------------------------------------------------
# System dependent paths
# ----------------------------------------------------
### oserl use eDoc for documentation, to regenerate update paths as needed!
ifneq ($(wildcard /usr/lib/erlang/lib/edoc*),)
  EDOC_APP = $(wildcard /usr/lib/erlang/lib/edoc*) 
else
  ifneq ($(wildcard /Library/DarwinPorts/lib/erlang/lib/edoc*),)
    EDOC_APP = $(wildcard /Library/DarwinPorts/lib/erlang/lib/edoc*)
  endif
endif
ifneq ($(wildcard /usr/lib/erlang/lib/xmerl*),)
  XMERL_APP = $(wildcard /usr/lib/erlang/lib/xmerl*)
else
  ifneq ($(wildcard /Library/DarwinPorts/lib/erlang/lib/xmerl*),)
    XMERL_APP = $(wildcard /Library/DarwinPorts/lib/erlang/lib/xmerl*)
  endif
endif
ifneq ($(wildcard /usr/lib/erlang/lib/syntax_tools*),)
  SYNTAX_TOOLS_APP = $(wildcard /usr/lib/erlang/lib/syntax_tools*)
else
  ifneq ($(wildcard /Library/DarwinPorts/lib/erlang/lib/syntax_tools*),)
    SYNTAX_TOOLS_APP = $(wildcard /Library/DarwinPorts/lib/erlang/lib/syntax_tools*)
  endif
endif

EDOC_PATHS = \
	-pa $(EDOC_APP)/ebin -pa $(XMERL_APP)/ebin -pa $(SYNTAX_TOOLS_APP)/ebin

RELEASE_PATH = /usr/local/lib/erlang
