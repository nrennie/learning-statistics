# load packages
library(markdown)
library(tidyverse)
library(usefunc)
library(showtext)
library(emojifont)
library(reactable)
library(shiny)
library(here)

# load modules
source(paste0(here::here(), "/moduleDescriptive.R"))
source(paste0(here::here(), "/moduleDistributions.R"))
source(paste0(here::here(), "/moduleConfidence.R"))
source(paste0(here::here(), "/moduleRegression.R"))
source(paste0(here::here(), "/moduleTtest.R"))

# fonts
font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "slab")
showtext_auto()

# make data
set.seed(1234)
population_df = tibble(ID = 1:200,
                       x = rep(1:20, times = 10),
                       y = rep(1:10, each = 20),
                       Value = rnorm(200, 50, 12))

# mode function
mode_avg <- function(data) {
  unique_values <- unique(data)
  unique_values[which.max(tabulate(match(data, unique_values)))]
}

# is inf
is_inf <- function(x) {
  if (x %in% c(-Inf, Inf)) {
    return(NA)
  } else {
    return(x)
  }
}
