#!/bin/bash

cd /Users/geofflambeth/OneDrive/GitHub/COVID-19app/
git checkout automated
git merge upstream/master

git checkout automated
git checkout --patch StableREADME README.md

cd /Users/geofflambeth/OneDrive/GitHub/COVID-19app/ShinyApp
/usr/local/bin/Rscript Update.R
