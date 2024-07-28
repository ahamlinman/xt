dist/xt-%.tar.gz: dist/xt-%
	tar -czvf $@ -C $(<D) $(<F)

.PRECIOUS: dist/xt-%

dist/xt-%: doc/RELEASE-README.txt doc/xt.1 doc/LICENSES.html target/%/release-opt/xt
	rm -rf $@
	mkdir -p $@
	cp $^ $@/

doc/LICENSES.html: Cargo.lock about.hbs
	cargo about generate about.hbs > $@

target/%/release-opt/xt:
	cargo build --profile release-opt --target $*

.PHONY: clean

clean:
	rm -rf dist target doc/LICENSES.html
