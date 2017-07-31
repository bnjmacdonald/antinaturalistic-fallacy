#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo $DIR
cd $DIR

cd ../analysis
Rscript main_analysis.r

cd ../paper
prince antinaturalistic-fallacy.html -o antinaturalistic-fallacy.pdf --javascript

cd -
