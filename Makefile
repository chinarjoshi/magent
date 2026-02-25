EMACS ?= emacs
BATCH = $(EMACS) --batch -Q -L . \
	--eval "(package-initialize)"

.PHONY: test compile clean

test:
	$(BATCH) -l test/test-magent-core.el -l test/test-magent-backend.el -l test/test-magent-integration.el -f ert-run-tests-batch-and-exit

compile:
	$(BATCH) -f batch-byte-compile magent-core.el magent-backend.el magent-ui.el magent.el

clean:
	rm -f *.elc
