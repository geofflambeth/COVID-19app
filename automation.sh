#!/bin/bash

cd /Users/geofflambeth/OneDrive/GitHub/COVID-19app/
git checkout automated
git merge --no-ff --no-commit upstream/master
git reset HEAD README.md
git checkout -- README.md
git commit -m "Automatically merged upstream/master"

cd /Users/geofflambeth/OneDrive/GitHub/COVID-19app/ShinyApp
/usr/local/bin/Rscript Update.R
