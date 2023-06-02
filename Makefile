.PHONY: test
test: docsim.el docsim-test.el
	emacs -Q -batch \
	-l ert \
	-l docsim-test.el \
	--eval "(setq ert-batch-backtrace-right-margin 500)" \
	-f ert-run-tests-batch-and-exit
