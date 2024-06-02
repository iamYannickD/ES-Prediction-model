# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages
p_load(tidyverse, ggridges)

# select country of interest 
cntry <- "NIGERIA"

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
  filter(!is.na(COUNTRY), COUNTRY == cntry) |>
  select(1:15, IST, SITE_NAME, Long_X, Lat_Y) |>
  mutate(
    numb_days = as.integer(dmy(Datesampleinlab) - dmy(Dateofcollection)),
    year = year(dmy(Datesampleinlab)),
    ep_week = as.numeric(epiweek(dmy(Datesampleinlab))),
    ep_month = as.numeric(month(dmy(Datesampleinlab)))
  )  

# analysis by province =====

# computation of the time to reach the lab
es_data_prepared <-
  es_data |> 
  filter(numb_days >= 0) |> 
  group_by(Province, year) |> # value by year/ month / week ? year ep_month/ ep_week
  #summarise(median_days = median(numb_days), max_days = max(numb_days) , .groups = "drop") |>
  mutate(
    median_days = median(numb_days),
    max_days = max(numb_days)) |>
  filter(!is.na(median_days)) |> # filter out sites with no median value
  mutate(
    ep_week = as.factor(ep_week),
    #ep_month = as.factor(ep_month),
    median_cat = case_when(
      median_days <= 3 ~ 1,
      (median_days > 3 & median_days <= 5) ~ 2,
      median_days > 5 ~ 3)
  ) |>
  ungroup() |> 
  mutate(median_stat = factor(median_cat, labels = c("<= 3", "3 - 5", "> 5")))

cats_count <- es_data_prepared |> 
  group_by(year, median_stat) |> 
  summarise(count = n_distinct(Sitename)) |> 
  # create columns based on the category and fill them by using valies from the count
  pivot_wider(names_from = median_stat, values_from = count, values_fill = 0) |> 
  # prepare a string that will have the number of sites (count) by category and by year
  mutate(
    annotation = paste0("<= 3: ", `<= 3`, "\n3 - 5: ", `3 - 5`, "\n> 5: ", `> 5`)
  )
days_to_lab <-
    ggplot(data = es_data_prepared) +
      geom_density_ridges(aes(x = median_days, y = Province, fill = median_stat ), 
                          scale = 1, rel_min_height = 0.01, linewidth = 0.2 ) + # rel_min_height hides values less than 0.01
      geom_vline(xintercept = 3, linetype = "dotted", color = "red") + # For a vertical dot line at x = 3  
      scale_fill_manual(values = c("<= 3" = "green", "3 - 5" = "yellow", "> 5" = "red")) +
      facet_wrap(~year) +
      labs(title = paste0("Number of Days to reach the lab by Provinces in ", str_to_title(cntry)),
           x = "Number of Days",
           y = "Provinces",
           fill = "Median Days") +
      scale_x_continuous(limits = c(-0.5, NA)) +  # For the x-axis to start at -0.5
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5), # 0 orientation, half way from the division
            axis.text.y = element_text(size = 5),
            plot.title = element_text(hjust = 0.5, face = "bold"),) + # ajust and bold the title
      geom_label(
        data = cats_count,
        aes(x = Inf, y = -Inf, label = annotation), # inf for the annotation to be even outside the graph
        hjust = 1.2, vjust = -0.5, label.padding = unit(0, "lines"), label.size = 0, # position of annotation
        inherit.aes = FALSE,
        size = 2.5, fill = "white",
        color = "black")   #+ coord_cartesian(clip = 'off')

# export 
ggsave(filename = paste0("../data/outputs/", str_to_title(cntry), "_median_days_to_lab.png"), plot = days_to_lab, width = 10, height = 8, dpi = 300)

# computation of the ev rate
ev_rate_data_prep <-
  es_data |>
  filter(numb_days >= 0) |>
  mutate(
    positive_pv = as.integer( if_else(
      str_detect(Finalcellcultureresult, "^1") | str_detect(Finalcellcultureresult, "^3") |
        str_detect(Finalcellcultureresult, "^4"), 1, 0))
  ) |>
  group_by(Province, year) |> # value by year/ month / week ? year ep_month/ ep_week
  mutate(ev_rate = mean(positive_pv),
         ep_month = as.factor(ep_month),
         ev_isolation_status = 
           case_when(
             ev_rate < 0.25 ~ 1,
             (ev_rate >= 0.25 & ev_rate < 0.5) ~ 2,
             ev_rate >= 0.5 ~ 3)) |>
  filter(!is.na(ev_rate)) |>
  ungroup() |>
  mutate(ev_isolation_rate = factor(ev_isolation_status, labels = c("<0.25", "0.25 - 0.5", "> 0.5")))


cats_ev_count <- ev_rate_data_prep |> 
  group_by(year, ev_isolation_rate) |> 
  summarise(count = n_distinct(Sitename)) |> 
  # create columns based on the category and fill them by using valies from the count 
  pivot_wider(names_from = ev_isolation_rate, values_from = count, values_fill = 0) |>
  # prepare a string that will have the number of sites (count) by category and by year
  mutate(
    annotation = paste0("<0.25: ", `<0.25`, "\n0.25 - 0.5: ", `0.25 - 0.5`, "\n> 0.5: ", `> 0.5`)
  )
global_ev_rate <-
    ggplot(data = ev_rate_data_prep) +
      geom_density_ridges(aes(x = ev_rate, y = Province, fill = ev_isolation_rate), 
                          scale = 1, rel_min_height = 0.1, linewidth = 0.2 ) +
      geom_vline(xintercept = 0.5, linetype = "dotted", color = "red") +
      scale_fill_manual(values = c("<0.25" = "red", "0.25 - 0.5" = "yellow", "> 0.5" = "green")) +
      facet_wrap(~year) +
      labs(title = paste0("EV rate by year and by provinces in ", str_to_title(cntry)),
           x = "EV rate",
           y = "Provinces",
           fill = "Median Days") +
      scale_x_continuous(
        limits = c(-0.02, 1.1),
        breaks = c(0, 0.5, 1),  # Major ticks
        minor_breaks = c(0.25, 0.75)  # Minor ticks
        ) +  # Set the x-axis to start at 0
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_text(size = 5.5),
            plot.title = element_text(hjust = 0.5, face = "bold")) +
      geom_label(
        data = cats_ev_count,
        aes(x = Inf, y = -Inf, label = annotation),
        hjust = 1.12, vjust = -0.5, label.padding = unit(0, "lines"), label.size = 0,
        inherit.aes = FALSE,
        size = 2.5, fill = "white",
        color = "black")

ggsave(filename = paste0("../data/outputs/", str_to_title(cntry), "_global_ev_rate.png"), plot = global_ev_rate, width = 10, height = 8, dpi = 300)
