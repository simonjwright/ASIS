
include Makefile.stub

BLD=prod
COMPS=lib
GNSA=no
# lib/tools/toolsdev

# ==================================================== build

.PHONY: all
all: setup-snames
	$(GPRBUILD) -p -j0 -XBLD=$(BLD) -XOPSYS=$(OPSYS) -XGNSA=$(GNSA) \
		$(GPRBUILD_FLAGS) -XASIS_COMPONENTS=$(COMPS) build_asis.gpr

.PHONY: tools
tools: setup
	$(GPRBUILD) -p -j0 -XBLD=$(BLD) -XOPSYS=$(OPSYS) \
		$(GPRBUILD_FLAGS) -XASIS_COMPONENTS=tools build_asis.gpr

.PHONY: toolsdev
toolsdev: setup
	$(GPRBUILD) -p -j0 -XBLD=$(BLD) -XOPSYS=$(OPSYS) \
		$(GPRBUILD_FLAGS) -XASIS_COMPONENTS=toolsdev build_asis.gpr

# The setup target re-generates 4 generated Ada files. We avoid changing the
# timestamps of unchanged files, to avoid triggering recompilation of
# everything that depends on them. We first generate the files into (e.g.)
# snames.ns, and then if snames.ns and snames.ads differ, we do "mv snames.ns
# snames.ads". 'cmp' returns true (i.e. 0) if the files are identical.

.PHONY: setup
setup: setup-snames setup-factory

.PHONY: setup-snames
setup-snames: gnat/snames.ads-tmpl gnat/snames.adb-tmpl gnat/xutil.ads gnat/xutil.adb
	$(GPRBUILD) -p  -XBLD=$(BLD) $(GPRBUILD_FLAGS) gnat/snames.gpr
	cd gnat ; ./xsnamest$(exe_ext)
	if cmp --quiet gnat/snames.ns gnat/snames.ads; then \
	  echo "snames.ads unchanged"; \
	else \
	  echo "update snames.ads"; \
	  mv gnat/snames.ns gnat/snames.ads; \
	fi
	if cmp --quiet gnat/snames.nb gnat/snames.adb; then \
	  echo "snames.adb unchanged"; \
	else \
	  echo "update snames.adb"; \
	  mv gnat/snames.nb gnat/snames.adb; \
	fi

.PHONY: setup-factory
setup-factory:
	$(GPRBUILD) -p  -XBLD=$(BLD) $(GPRBUILD_FLAGS) \
		tools/tool_utils/generate_factory.gpr
	cd tools/tool_utils ; \
		./ada_trees-generate_factory$(exe_ext)
	if cmp --quiet tools/tool_utils/ada_trees-factory.ns tools/tool_utils/ada_trees-factory.ads; then \
	  echo "ada_trees-factory.ads unchanged"; \
	else \
	  echo "update ada_trees-factory.ads"; \
	  mv tools/tool_utils/ada_trees-factory.ns tools/tool_utils/ada_trees-factory.ads; \
	fi
	if cmp --quiet tools/tool_utils/ada_trees-factory.nb tools/tool_utils/ada_trees-factory.adb; then \
	  echo "ada_trees-factory.adb unchanged"; \
	else \
	  echo "update ada_trees-factory.adb"; \
	  mv tools/tool_utils/ada_trees-factory.nb tools/tool_utils/ada_trees-factory.adb; \
	fi

# ==================================================== install

.PHONY: install-clean
install-clean-legacy:
ifneq (,$(wildcard $(prefix)/lib/gnat/manifests/asislib))
	-$(GPRINSTALL) --uninstall --prefix=$(prefix) \
		--project-subdir=lib/gnat asislib
endif
ifneq (,$(wildcard $(prefix)/lib/gnat/manifests/build_asis))
	-$(GPRINSTALL) --uninstall --prefix=$(prefix) \
		--project-subdir=lib/gnat build_asis
endif

install-clean: install-clean-legacy
ifneq (,$(wildcard $(prefix)/share/gpr/manifests/asislib))
	-$(GPRINSTALL) --uninstall --prefix=$(prefix) asislib
endif
ifneq (,$(wildcard $(prefix)/share/gpr/manifests/build_asis))
	-$(GPRINSTALL) --uninstall --prefix=$(prefix) build_asis
endif

GPRINST_OPTS=-p -f --prefix=$(prefix) --sources-subdir=include/asis \
	--lib-subdir=lib/asis -XBLD=$(BLD) -XOPSYS=$(OPSYS) \
	--build-var=LIBRARY_TYPE --build-var=ASIS_BUILD --build-name=static

.PHONY: install
install: install-clean
	$(GPRINSTALL) $(GPRINST_OPTS) -XASIS_COMPONENTS=lib build_asis.gpr

.PHONY: install-tools-clean
install-tools-clean-legacy:
ifneq (,$(wildcard $(prefix)/lib/gnat/manifests/asistools))
	-$(GPRINSTALL) --uninstall --prefix=$(prefix) \
		--project-subdir=lib/gnat asistools
endif

install-tools-clean:
ifneq (,$(wildcard $(prefix)/share/gpr/manifests/asistools))
	-$(GPRINSTALL) --uninstall --prefix=$(prefix) asistools
endif

.PHONY: install-tools
install-tools: install-tools-clean
	$(GPRINSTALL) $(GPRINST_OPTS) --no-project \
		--install-name=asistools -XASIS_COMPONENTS=tools build_asis.gpr

# ==================================================== test

.PHONY: gnattest_hash_testing
gnattest_hash_testing:
	$(GPRBUILD) "-Pinternal/tools/gnattest/hash_testing/$@" "-XBLD=$(BLD)" "-XOPSYS=$(OPSYS)"

# ==================================================== clean

.PHONY: clean
clean:
	-$(GPRCLEAN) -XASIS_COMPONENTS=lib build_asis.gpr
	-$(GPRCLEAN) -XASIS_COMPONENTS=toolsdev build_asis.gpr
	-$(GPRCLEAN) tools/tool_utils/generate_factory.gpr
	rm -f tools/tool_utils/ada_trees-factory.ads tools/tool_utils/ada_trees-factory.adb
