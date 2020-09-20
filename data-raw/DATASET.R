## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)
library(readxl)
library(tidyverse)

dat <- read_excel("data-raw/result_table_systematic_review.xlsx")

traits <- dat %>% 
  select(author, year, akronym, method.name, scale, implementation.standard,
         contains("scope"), contains("includes"),
         free.text.description, `DOI/link`) %>% 
  mutate(ID = paste(author, year, sep =""))

use_data(traits)

#also add literature lsit
library(bib2df)

literature1 <- bib2df("data-raw/syst_review.bib")
literature2 <- bib2df("data-raw/apriori.bib")

literature <- bind_rows(literature1, literature2)

use_data(literature)
