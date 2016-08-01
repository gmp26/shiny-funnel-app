# rcljsdemo

This R package installs a shiny app that makes demonstrates htmlwidgets - some of which may be coded in clojurescript, some in javascript, and some in R. 

## Installing

Note that this is not a production ready package. The intention is to add further examples.

```
install.packages("devtools")
library("devtools")
devtools::install_git("https://github.com/gmp26/rcljsdemo")
```

## Running

```
rcljsdemo::run_app()
```

If you clone this repository, you will find some test CSV files to use in the app in `tests/testthat/fixtures`.
```
git clone https://github.com/gmp26/rcljsdemo.git
cd rcljsdemo/tests/testthat/fixtures
ls no-problems.csv
```
