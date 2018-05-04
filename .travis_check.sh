#!/bin/bash
if [ -z "${R_DEVEL}" ]; then R --silent -e "devtools::install_github('Rdatatable/data.table')"; fi
R CMD build .
R CMD check *tar.gz
