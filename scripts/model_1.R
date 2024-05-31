# Check if the package pacman is installed
  if (!require("pacman")) {install.packages("pacman")} 
  library("pacman")

# Load packages
  p_load(tidyverse, sf, ggrepel, officer)

# load all polio data
  polio_data <-
    read_rds("../data/polio_data.rds")
  
# load masterlist from the ES repository
  masterlist <-
        read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSu8KW76dhhUwyT3T_Dll4QK2ciORzTQZY9xCXoYrUjZPQ6AtWmjk0xPpYpIW84fg/pub?output=csv") |>
        filter(STATUS == "ACTIVE")
  
  
# load es data from polio_data
  es_data <-
        polio_data$es |>
        filter(region.who.code == "AFRO")


# load admin boundaries for AFRO
afro_Adm0 <- 
        polio_data$global.ctry |> 
        as_tibble() |>
        filter(WHO_REGION == "AFRO")

afro_Adm1 <- 
        polio_data$global.prov |> 
        as_tibble() |>
        filter(WHO_REGION == "AFRO")

afro_Adm2 <- 
        polio_data$global.dist |> 
        as_tibble() |>
        filter(WHO_REGION == "AFRO")











