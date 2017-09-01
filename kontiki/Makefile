PROTOS = kontiki.protocol.server.proto

COMPILE_PROTO_FILE = compile-proto-file
EVAL = stack exec --
AWK = awk

GENDIR = gen
PROTODIR = proto

default: all
.PHONY: default

namespace_to_path = $(subst .,/,$(shell \
	echo $(1:$(PROTODIR)/%=%) | \
	$(AWK) 'BEGIN {FS=OFS="."} {for(i=1;i<=NF;i++){ $$i=toupper(substr($$i,1,1)) substr($$i,2) }}1'))
hsmodule = $(GENDIR)/$(call namespace_to_path,$(1:.proto=)).hs

MODULES =

define MODULE_template =
$(call hsmodule,$1): $(1)
	@echo "Compiling $$<..."
	@cd $$(dir $$<) && $$(EVAL) $$(COMPILE_PROTO_FILE) --out $$(abspath $$(GENDIR)) --proto $$(notdir $$<)
	@echo "Disabling 'missing-import-lists' warning in $$@..."
	@$$(AWK) -i inplace '/-- | Generated/{print "{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}"}1' $$@ || rm -f $$@
MODULES += $(call hsmodule,$1)
endef

$(foreach proto,$(PROTOS),$(eval $(call MODULE_template,$(PROTODIR)/$(proto))))

all: $(MODULES)
.PHONY: all

clean:
	@rm -f $(strip $(MODULES))
.PHONY: clean
