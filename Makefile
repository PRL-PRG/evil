.PHONY: all build check document test

all: document build check

build: document
	R CMD build .

check: build
	R CMD check evil*tar.gz

clean:
	-rm -f evil*tar.gz
	-rm -fr evil.Rcheck
	-rm -rf src/*.o src/*.so

document:
	Rscript -e 'devtools::document()'

test:
	Rscript -e 'devtools::test()'

lintr:
	R --slave -e "lintr::lint_package()"
