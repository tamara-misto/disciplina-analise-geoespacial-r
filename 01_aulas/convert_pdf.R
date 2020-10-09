# -------------------------------------------------------------------------
# script convert to xaringan presentation to pdf

# packages
library(pagedown)
library(xaringan)
library(tidyverse)

# directory
setwd("01_aulas")
dir(pattern = ".Rmd")

# convert rmarkdown
purrr::map(dir(pattern = ".Rmd")[2], chrome_print, timeout = 2000)

# end ---------------------------------------------------------------------