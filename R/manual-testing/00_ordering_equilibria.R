##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2023-03-19
#'
#' This file contains manual testing for the equilibria matching process
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

all_arams_unordered <- readr::read_csv(
  here::here(
    "./data/parameter-data/all-parameter-vals-equilibria-unordered.csv"))