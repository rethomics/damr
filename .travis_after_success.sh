#!/bin/bash
if [ -z "${R_RELEASE}" ]
then R -e 'covr::codecov()'
fi
