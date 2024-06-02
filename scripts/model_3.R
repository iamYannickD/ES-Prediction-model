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
      es_data_list[[paste0("es_", years[i])]] <- read_csv(data_links[i])
    }
    
# Assign datasets to variables by year
    list2env(es_data_list, .GlobalEnv)
  
# load masterlist
    masterlist <- es_repo$lien[1] 
    
    active_es_sites <-
        read_csv(masterlist) |>
        filter(STATUS == "ACTIVE") 
    
# List of common columns to select
  common_columns <- c("Countryname", "Province", "District", "Sitename", "Sitecode", 
                      "Dateofcollection", "Datesamplesenttolab", "Datesampleinlab", 
                      "Samplecondition", "Finalcellcultureresult", "Datefinalcultureresult")
  
# Handle the specific column name for Sitecode1 in es_2019 separately
  columns_2019 <- c("Countryname", "Province", "District", "Sitename", "Sitecode...22", 
                    "Dateofcollection", "Datesamplesenttolab", "Datesampleinlab", 
                    "Samplecondition", "Finalcellcultureresult", "Datefinalcultureresult")
  
# Apply select to each dataset using columns names, and save result in a list
# as the number of columns are not equal, the idea is to select some specific columns that we will use in the 
  # analysis. Then, to avoid repeiting the same script, we create 2 variables filled with columns names and that
  # we will use in the select script. Then, we use the for loop to iterate our selection on all the samples
  for (year in names(es_data_list)) {
    if (year == "es_2019") {
      es_data_list[[year]] <- es_data_list[[year]] %>%
        select(all_of(columns_2019)) |>
        rename(Sitecode = Sitecode...22)
    } else {
      es_data_list[[year]] <- es_data_list[[year]] %>%
        select(all_of(common_columns))
    }
  }
  
# Assign datasets to variables by year
  list2env(es_data_list, .GlobalEnv)

# create a cumulative es file with all the results generated 
  all_es_data <-
    bind_rows(es_2016_2017, es_2018, es_2019, es_2020, es_2021, es_2022, es_2023, es_2024)

# adding long lat to ES sites and other information from the masterlist
  es_data <-
    left_join(x = all_es_data, y = active_es_sites, by = c("Sitecode" = "SITE_CODE" )) |>
      filter(!is.na(COUNTRY), COUNTRY == "CAMEROON") |>
    select(1:15, IST, SITE_NAME, Long_X, Lat_Y) |>
    mutate(
      numb_days = as.integer(dmy(Datesampleinlab) - dmy(Dateofcollection)),
      year = year(dmy(Datesampleinlab)),
      ep_week = as.numeric(epiweek(dmy(Datesampleinlab))),
      ep_month = as.numeric(month(dmy(Datesampleinlab)))
    )  
  
plot_1 <- es_data |> 
    filter(numb_days >= 0) |> 
    group_by(COUNTRY, ep_month) |>
    #summarise(median_days = median(numb_days), max_days = max(numb_days) , .groups = "drop") |>
    mutate(
      median_days = median(numb_days),
      max_days = max(numb_days)) |>
      mutate(
        ep_week = as.factor(ep_week),
        ep_month = as.factor(ep_month)) |>
      ungroup() |> 
    ggplot() +
    #geom_boxplot(aes(x = numb_days, y = fct_reorder(PROVINCE, median_days))) 
    #geom_density_ridges(aes(x = median_days, y = ep_week, fill = median_days ), scale = 0.9 ) + 
    geom_density_ridges(aes(x = median_days, y = ep_month, fill = median_days ), scale = 1 ) +
    facet_wrap(~year) +
    labs(title = "Number of Days by Median Days and Months",
         x = "Number of Days",
         y = "Month",
         fill = "Median Days") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
plot_1

#mat_es_months <-
  es_data |>
  filter(numb_days >= 0) |>
  mutate(
    positive_pv = as.integer( if_else(
                    str_detect(Finalcellcultureresult, "^1") | str_detect(Finalcellcultureresult, "^3") |
                    str_detect(Finalcellcultureresult, "^4"), 1, 0))
            ) |>
  group_by(Sitecode, ep_month) |>
  mutate(ev_rate = mean(positive_pv),
         ep_month = as.factor(ep_month),
         ev_isolation_status = 
                     case_when(
                       ev_rate < 0.25 ~ 1,
                       (ev_rate >= 0.25 & ev_rate < 0.5) ~ 2,
                       ev_rate >= 0.5 ~ 3)) |>
    filter(!is.na(ev_rate)) |>
    ungroup() |>
    mutate(ev_isolation_rate = factor(ev_isolation_status, labels = c("<0.25", "0.25 - 0.5", "> 0.5"))) |>
  ggplot() +
  geom_density_ridges(aes(x = ev_rate, y = ep_month, fill = ev_isolation_rate), scale = 1 ) +
  facet_wrap(~year) +
  labs(title = "Number of Days by Median Days and Months",
       x = "Number of Days",
       y = "Month",
       fill = "Median Days") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  
  
  