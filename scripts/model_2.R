# Check if the package pacman is installed
if (!require("pacman")) {install.packages("pacman")} 
library("pacman")

# Load packages
p_load(tidyverse, sf, ggrepel, officer, ggridges)

# load all polio data
polio_data <-
  read_rds("../data/polio_data.rds")

polio_data_2016 <-
  read_rds("../data/polio_data.2016.2018.rds")

# load masterlist from the ES repository
my_link <- read_csv("../data/link/access.txt")
link <- my_link$lien[1]

active_es_sites <-
  read_csv(link) |>
  filter(STATUS == "ACTIVE")


# load es data from polio_data
raw_es_data <-
  bind_rows(polio_data$es, polio_data_2016$es) |>
  filter(region.who.code == "AFRO")


# load admin boundaries for AFRO
afro_Adm0 <- 
  polio_data$global.ctry |> 
  as_tibble() |>
  filter(WHO_REGION == "AFRO" & ADM0_NAME == "NIGERIA")

afro_Adm1 <- 
  polio_data$global.prov |> 
  as_tibble() |>
  filter(WHO_REGION == "AFRO" & ADM0_NAME == "NIGERIA")

afro_Adm2 <- 
  polio_data$global.dist |> 
  as_tibble() |>
  filter(WHO_REGION == "AFRO" & ADM0_NAME == "NIGERIA")

# adding long lat to ES sites
es_data <-
  left_join(x = raw_es_data, y = active_es_sites, by = c("site.code" = "SITE_CODE" )) |>
  filter(!is.na(COUNTRY)) |>
  select(IST, ADM0_NAME, COUNTRY, PROVINCE, DISTRICT_ADM02_NAME, SITE_NAME, Lat_Y, Long_X, collection.date, date.shipped.to.ref.lab, date.received.in.lab ) |>
  mutate(
    numb_days = as.integer(dmy(date.received.in.lab) - dmy(collection.date)),
    year = year(dmy(date.received.in.lab)),
    ep_week = as.numeric(epiweek(dmy(date.received.in.lab)))
  ) |>
  mutate(
    time_to_reach_lab = 
      case_when(
        numb_days <= 3 ~ "< 3 days",
        numb_days >= 3 & numb_days <= 5 ~ "> 3 and <= 5 days",
        numb_days > 5 & numb_days <= 10 ~ "> 5 and <=10 days",
        numb_days > 10 ~ "> 10 days"
      ) 
  ) |>
  #convert the categories (double) into factors for the classification
  mutate(time_to_reach_lab = factor(time_to_reach_lab, 
                                    labels = c("< 3 days", "> 3 and <= 5 days", "> 5 and <=10 days", "> 10 days"))) |>
  filter(COUNTRY == "SOUTH SUDAN")

es_data |> 
      filter(numb_days >= 0) |> 
      group_by(COUNTRY, ep_week) |>
      #summarise(median_days = median(numb_days), max_days = max(numb_days) , .groups = "drop") |>
      mutate(
        median_days = median(numb_days),
        max_days = max(numb_days)) |>
      mutate(
        ep_week = as.factor(ep_week)) |>
      ungroup() |> 
      ggplot() +
      #geom_boxplot(aes(x = numb_days, y = fct_reorder(PROVINCE, median_days))) 
      geom_density_ridges(aes(x = median_days, y = ep_week, fill = median_days ), scale = 0.9 ) + 
      facet_wrap(~year) +
  labs(title = "Number of Days by Median Days and Months",
       x = "Number of Days",
       y = "Month",
       fill = "Median Days") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

d














# plot a map with location of es sites and # days it takes to reach the lab
#plot1 <- 
ggplot() +
  geom_sf(data = afro_Adm1, aes(geometry = SHAPE), fill = NA, color = "gray") +
  geom_sf(data = afro_Adm0, aes(geometry = Shape), fill = NA, color = "black", size = 1) +
  geom_point(data = es_data, aes(x = Long_X, y = Lat_Y, size = 5, fill = numb_days, color = numb_days),
             size = 1.5, stroke = 1) +
  #scale_fill_gradient(low = "green", high = "red", name = "numb_days") +
  #scale_color_gradient(low = "green", high = "red", name = "numb_days")
  
  scale_fill_gradient(values = c("< 3 days" = "green", "> 3 and <= 5 days" = "yellow", "> 5 and <=10 days" = "red", "> 10 days" = "darkred"), name = "numb_days") +
  scale_color_gradient(values = c("< 3 days" = "green", "> 3 and <= 5 days" = "yellow", "> 5 and <=10 days" = "red", "> 10 days" = "darkred"), name = "numb_days") +
  labs(x = "Longitude", y = "Latitude", title = "ES sites by time it takes to reach the lab")  +
  
  theme_bw()









