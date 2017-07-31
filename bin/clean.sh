#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo $DIR
cd $DIR
cd ../analysis

Rscript cleaning_wave1.r
Rscript cleaning_wave2.r
Rscript cleaning_wave3.r
Rscript merging_waves.r