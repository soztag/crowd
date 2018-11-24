# knitr options ====
knitr::opts_chunk$set(
  tidy = TRUE,   # tidy formats code nicely in echo
  cache = TRUE,
  echo = FALSE,
  message = FALSE
)
options(digits = 1)  # display only 1 decimal in knitr output
options(scipen = 999)
options(knitr.kable.NA = 'Keine Angabe oder Fehlwert')

# dependencies ====
library(tidyverse)
library(checkmate)
library(dplyr)
library(magrittr)
library(purrr)
library(forcats)