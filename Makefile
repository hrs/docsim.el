.PHONY: test
test: docsim.el docsim-test.el
	emacs --quick --batch \
	-f package-initialize \
  --eval "(add-to-list 'package-archives '(\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\") t)" \
  --eval "(when (not (package-installed-p 'buttercup)) (package-refresh-contents) (package-install 'buttercup))" \
	-l buttercup \
	-l org \
	-l org-element \
	-L . \
	-f buttercup-run-discover

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
