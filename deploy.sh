#!/bin/bash

cd ~/norsyss
make -B fix_description_version
Rscript -e "setwd('~/norsyss'); devtools::check()"
Rscript -e "setwd('~/norsyss'); devtools::build()"
rm ~/norsyss_*.tar.gz
Rscript -e "setwd('~/norsyss'); pkgdown::build_site()"
make -B pkgdown

