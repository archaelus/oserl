# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------
APPNAME = oserl
VSN = 1.2

# FIXME These variables should be automatically set when compiling
ERL = erl
ERLC = erlc
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
EDOC_APP = /usr/local/lib/erlang/lib/edoc-0.4
XMERL_APP = /usr/local/lib/erlang/lib/xmerl-0.19
SYNTAX_TOOLS_APP = /usr/local/lib/erlang/lib/syntax_tools-1.3

EDOC_PATHS = \
	-pa $(EDOC_APP)/ebin -pa $(XMERL_APP)/ebin -pa $(SYNTAX_TOOLS_APP)/ebin

RELEASE_PATH = /usr/local/lib/erlang
