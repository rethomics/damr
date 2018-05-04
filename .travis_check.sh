#!/bin/bash
echo $R_DEVEL
if [[ "${R_DEVEL}" ]]; then R --silent -e "devtools::install_github('Rdatatable/data.table')"; fi
R CMD build .
R CMD check *tar.gz
