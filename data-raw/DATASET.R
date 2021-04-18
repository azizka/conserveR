## code to prepare `DATASET` dataset goes here

# usethis::use_data(DATASET, overwrite = TRUE)
library(readxl)
library(tidyverse)
library(usethis)

traits <- read_excel("data-raw/traits.xlsx")

use_data(traits, overwrite = TRUE)

#also add literature lsit
library(bib2df)
library(usethis)

literature1 <- bib2df("data-raw/syst_review.bib")
literature2 <- bib2df("data-raw/apriori.bib")

literature <- bind_rows(literature1, literature2)

#change BIBTEX KEY to author year
literature <- literature %>% 
  mutate(BIBTEXKEY = paste(str_split_fixed(BIBTEXKEY,n = 3, pattern = "_")[,1],
                    str_split_fixed(BIBTEXKEY,n = 3, pattern = "_")[,3],
                    sep = "")) %>% 
  mutate(BIBTEXKEY = ifelse(BIBTEXKEY == "sanchezdios_role_2017", "sanchezdedios2017", BIBTEXKEY)) %>% 
  mutate(BIBTEXKEY = ifelse(BIBTEXKEY == "vanapplication_2014", "vanzonneveld2014", BIBTEXKEY))%>% 
  mutate(BIBTEXKEY = ifelse(BIBTEXKEY == "gaspar-cunha2015", "schlottfeldt2015", BIBTEXKEY))


use_data(literature, overwrite = TRUE)

# the MCA and network results
load("data-raw/mca_results.rda")
use_data(mca_results, overwrite = TRUE)

load("data-raw/connectivity_network.rda")
use_data(connectivity_network, overwrite = TRUE)
