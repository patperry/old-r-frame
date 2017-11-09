RSCRIPT= Rscript --vanilla

all: README.md

NEWS: NEWS.md
	sed -e 's/^### //g; s/`//g' $< > $@

README.md: README.Rmd
	$(RSCRIPT) -e 'devtools::load_all(); knitr::knit("README.Rmd")'

check:
	$(RSCRIPT) -e 'devtools::test(".")'

clean:
	$(RSCRIPT) -e 'devtools::clean_dll(".")' && rm -f README.md

cov:
	$(RSCRIPT) -e 'covr::package_coverage(line_exclusions = "R/deprecated.R")' 
dist: $(BUILT_VIGNETTES) NEWS README.md
	mkdir -p dist && cd dist && R CMD build ..

doc: NEWS README.md

install:
	$(RSCRIPT) -e 'devtools::install()'

site:
	$(RSCRIPT) -e 'pkgdown::build_site()'

.PHONY: all check clean con dist distclean doc install site
