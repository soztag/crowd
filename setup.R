# knitr options ====
knitr::opts_chunk$set(
  tidy = TRUE,   # tidy formats code nicely in echo
  cache = TRUE,
  echo = FALSE,
  message = FALSE
)
options(digits = 2)  # display only 2 digits in knitr output
options(scipen = 999)
options(knitr.kable.NA = '')

# dependencies ====
library(tidyverse)
library(checkmate)
library(dplyr)
library(magrittr)
library(purrr)
library(forcats)
