##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2023-03-15
#'
#' This targets file contains all targets to run this analysis. If you are not
#' familiar with the targets package, see the documentation here:
#' https://books.ropensci.org/targets/
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

library(targets)
library(tarchetypes)
library(here)

# read in functions files


tar_option_set(packages = c("readr"),
               error = "stop")


list(
    # files ====================================================================
    tar_target(
        unsorted_param_combos,
        here::here(
            "./data/parameter-data/all-parameter-vals-equilibria-unordered.csv"
            )
    )
)