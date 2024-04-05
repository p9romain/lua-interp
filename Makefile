all: ocaml rust rustrelease

ocaml:
	cd ocaml; dune build

rust:
	cd rust; cargo build

rustrelease:
	cd rust; cargo build --release

refinterp:
	$(MAKE) -C lua all

tests: refinterp ocaml rustrelease
	ocaml -I +unix unix.cma tests/runtests.ml tests/

test: refinterp ocaml rustrelease
	ocaml -I +unix unix.cma tests/runtests.ml $(TEST)

clean:
	cd ocaml; dune clean
	cd rust; cargo clean; rm -f Cargo.lock
	$(MAKE) -C lua clean

.PHONY: all ocaml rust rustrelease refinterp test clean
