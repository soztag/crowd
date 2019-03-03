# knitr options ====
knitr::opts_chunk$set(
  tidy = TRUE,   # tidy formats code nicely in echo
  cache = TRUE,
  echo = FALSE,
  message = FALSE
)
options(digits = 1)  # display only 1 decimal in knitr output
options(scipen = 999)
options(knitr.kable.NA = 'Keine Angabe o. Fehlwert')

# image sizes for latex ===
if (isTRUE(getOption('knitr.in.progress')) && knitr::opts_knit$get("rmarkdown.pandoc.to") == "latex") {
  knitr::opts_chunk$set(
    out.width = "1\\linewidth"
  )
}

# dependencies ====
library(tidyverse)
library(checkmate)
library(dplyr)
library(magrittr)
library(purrr)
library(forcats)
