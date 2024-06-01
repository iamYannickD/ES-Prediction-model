# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages
p_load(tidyverse, sf, ggrepel, officer, ggridges)

# connect to all repositories
es_repo <- read_csv("../data/link/access.txt")
  es_2016_2017 <-  es_repo$lien[2]
  es_2018 <-  es_repo$lien[3]
  es_2019 <-  es_repo$lien[4]
  es_2020 <-  es_repo$lien[5]
  es_2021 <-  es_repo$lien[6]
  es_2022 <-  es_repo$lien[7]
  es_2023 <-  es_repo$lien[8]
  es_2024 <-  es_repo$lien[9]
  
  masterlist <- es_repo$lien[1] 
  
# load dataset
  active_es_sites <-
    read_csv(masterlist) |>
    filter(STATUS == "ACTIVE") 
  
  es_2016 <-
    read_csv(es_2016_2017)
  
  es_2018 <-
    read_csv(es_2018)
  
  es_2019 <-
    read_csv(es_2019)
  
  es_2020 <-
    read_csv(es_2020)
  
  es_2021 <-
    read_csv(es_2021)
  
  es_2022 <-
    read_csv(es_2022)
  
  es_2023 <-
    read_csv(es_2023)
  
  es_2024 <-
    read_csv(es_2024)
  