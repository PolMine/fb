library(testthat)
library(PartyPages)

Sys.setenv(CORPUS_REGISTRY = path.expand("~/Data/cwb/registry"))
library(polmineR)

test_check("PartyPages")
