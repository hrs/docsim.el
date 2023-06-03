.PHONY: test
test: docsim.el docsim-test.el
	emacs -Q -batch \
	-l ert \
	-l org \
	-l org-element \
	-l docsim-test.el \
	--eval "(setq ert-batch-backtrace-right-margin 500)" \
	-f ert-run-tests-batch-and-exit
