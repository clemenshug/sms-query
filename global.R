library(magrittr)
library(dplyr)
library(data.table)
library(readr)
library(fst)
library(here)
library(htmltools)
library(markdown)
library(glue)
library(purrr)
library(rlang)
library(morgancpp)
library(yonder)
library(stringr)
library(DT)
library(shinycssloaders)
library(writexl)
library(shinyjs)

source("utils/similarity_funs.R", local = TRUE)

# Components used in multiple apps
source("modules/components/download_buttons.R", local = TRUE)

# Modules
source("modules/similarity_query.R", local = TRUE)

source("data/load.R", local = TRUE)

source("app_ui.R", local = TRUE)


# enableBookmarking(store = "url")

