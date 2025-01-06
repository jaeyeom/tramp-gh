EMACS ?= emacs

TEST_DIR=$(shell pwd)/test

# Run all tests by default.
MATCH ?=

.PHONY: build test lint checkdoc clean

build:
	$(EMACS) -Q --batch -L . -f batch-byte-compile tramp-gh.el

test:
	$(EMACS) --batch -L . -L $(TEST_DIR) -l test-tramp-gh.el -eval '(ert-run-tests-batch-and-exit "$(MATCH)")'

NEEDED_PACKAGES=package-lint

INIT_PACKAGE_EL="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (setq package-check-signature nil) \
  (package-initialize) \
  (package-refresh-contents) \
  (dolist (pkg '($(NEEDED_PACKAGES))) \
    (unless (package-installed-p pkg) \
      (package-install pkg))))"

lint:
	$(EMACS) -Q --batch \
		--eval $(INIT_PACKAGE_EL) \
		-l package-lint \
		-f package-lint-batch-and-exit \
		tramp-gh.el

checkdoc:
	$(EMACS) -Q --batch \
		--eval $(INIT_PACKAGE_EL) \
		-l checkdoc \
		--eval "(progn (checkdoc-file \"tramp-gh.el\") (princ \"\\nCheckdoc complete\\n\"))"

clean:
	rm -f *.elc test/*.elc
