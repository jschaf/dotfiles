# Ensure Emacs always runs from this makefile's PWD
EMACS_FLAGS=--eval '(setq user-emacs-directory default-directory)' -l emacs/start.el
EMACS=emacs --quick --batch $(EMACS_FLAGS)
EMACSI=emacs --quick $(EMACS_FLAGS)


test:
	@$(EMACS) -f abn/run-all-tests
