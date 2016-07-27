# funnel

This R package installs a shiny app that makes funnel plots or sliced funnel plots.

## Installing

Note that this is not a production ready package. The intention is to add further examples.

```
install.packages("devtools")
library("devtools")
devtools::install_git("https://github.com/gmp26/funnel")
```

## Running

```
funnel::run_app()
```

If you clone this repository, you will find some test CSV files to use in the app in `tests/testthat/fixtures`.
```
git clone https://github.com/gmp26/funnel.git
cd funnel/tests/testthat/fixtures
ls no-problems.csv
```
