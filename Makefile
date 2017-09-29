VERSION="$(shell sed -nre '/^;; Version:/ { s/^;; Version:[ \t]+//; p }' state-machine.el)"

default:
	@echo "Version: state-machine-$(VERSION)"

test: test-all

test-all:
	cask exec buttercup -L . test
