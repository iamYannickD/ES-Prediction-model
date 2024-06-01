# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages
p_load(tidyverse, sf, ggrepel, officer, ggridges)

# Load the repository containing the links
es_repo <- read_csv("../data/link/access.txt")

# load es sites from link
    data_links <- es_repo$lien[2:9]
    years <- c("2016_2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024")
    
    # Create an empty list to store datasets
    es_data_list <- list()
    
    # Load datasets using a for loop
    for (i in seq_along(years)) {
      es_data_list[[years[i]]] <- read_csv(data_links[i])
    }
    
    # Assign datasets to variables by year
    list2env(es_data_list, .GlobalEnv)
  
# load masterlist
    masterlist <- es_repo$lien[1] 
    
    active_es_sites <-
        read_csv(masterlist) |>
        filter(STATUS == "ACTIVE") 
  
  

  
  
  
  
  
  
  
  