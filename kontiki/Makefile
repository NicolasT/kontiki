COMPILE_PROTO_FILE = compile-proto-file
EVAL = stack exec --
AWK = awk

SRCDIR = src
PROTO = Kontiki/Protocol/Server.proto
MODULE = $(PROTO:.proto=.hs)

default: all

all: $(SRCDIR)/$(MODULE)

$(SRCDIR)/%.hs: $(SRCDIR)/%.proto
	@echo "Compiling $<..."
	@cd $(SRCDIR) && $(EVAL) $(COMPILE_PROTO_FILE) --out . --proto $(<:$(SRCDIR)/%=%)
	@echo "Disabling 'missing-import-lists' warning..."
	@$(AWK) -i inplace '/-- | Generated/{print "{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}"}1' $@ || rm -f $@

clean:
	@rm -f $(SRCDIR)/$(MODULE)
.PHONY: clean
