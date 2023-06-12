.PHONY: test
test: docsim.el docsim-test.el
	emacs --quick --batch \
	-l ert \
	-l org \
	-l org-element \
	-l docsim-test.el \
	--eval "(setq ert-batch-backtrace-right-margin 10000)" \
	-f ert-run-tests-batch-and-exit

.PHONY: lint
lint: byte_compile package_lint

.PHONY: byte_compile
byte_compile: docsim.el
	emacs --quick --batch \
	--eval "(setq byte-compile-error-on-warn t)" \
	--eval "(byte-compile-file \"docsim.el\")"

.PHONY: package_lint
package_lint: docsim.el
	emacs --quick --batch \
	-f package-initialize \
	-l package-lint \
	-f package-lint-batch-and-exit \
	docsim.el
