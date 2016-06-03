SHELL := /bin/bash

.PHONY: bootstrap
bootstrap:
	if [ -e $(HOME)/.emacs ]; then mv $(HOME)/.emacs{,.bak}; fi
	ln -s $(shell pwd)/dot-emacs $(HOME)/.emacs
