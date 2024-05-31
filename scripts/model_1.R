# Check if the package pacman is installed
  if (!require("pacman")) {install.packages("pacman")} 
  library("pacman")

# Load packages
  p_load(tidyverse, sf, ggrepel, officer, ggridges)

# load all polio data
  polio_data <-
    read_rds("../data/polio_data.rds")
  
# load masterlist from the ES repository
  active_es_sites <-
        read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSu8KW76dhhUwyT3T_Dll4QK2ciORzTQZY9xCXoYrUjZPQ6AtWmjk0xPpYpIW84fg/pub?output=csv") |>
        filter(STATUS == "ACTIVE")
  
  
# load es data from polio_data
  raw_es_data <-
        polio_data$es |>
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
    select(IST, COUNTRY, PROVINCE, DISTRICT_ADM02_NAME, SITE_NAME, Lat_Y, Long_X, collection.date, date.shipped.to.ref.lab, date.received.in.lab ) |>
    mutate(
      numb_days = as.integer(dmy(date.received.in.lab) - dmy(collection.date)),
      year = year(dmy(collection.date)),
      ep_week = epiweek(dmy(collection.date))
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
    filter(COUNTRY == "NIGERIA")
  
# prediction of the behavious for the next 1 month
  predict_time <-
          es_data |>
          select(SITE_NAME, numb_days, ep_week, year) |>
          arrange((SITE_NAME))
  
  es_data |> 
    filter(numb_days > 0, year > 2019) |> 
    group_by(PROVINCE) |>
    mutate(median_days = median(numb_days)) |> 
    ungroup() |> 
    # mutate(DISTRICT_ADM02_NAME = factor(DISTRICT_ADM02_NAME)) |> 
    # mutate(DISTRICT_ADM02_NAME = fct_reorder(DISTRICT_ADM02_NAME, mean_days)) |>
    # pull(DISTRICT_ADM02_NAME)
    ggplot() +
    #geom_boxplot(aes(x = numb_days, y = fct_reorder(PROVINCE, median_days))) 
    geom_density_ridges(aes(x = numb_days, y = fct_reorder(PROVINCE, median_days))) + 
    facet_wrap(~year)
  
  # prediction  glm_model <- glm(numb_days ~ year , family = "poisson", data = es_data |> filter(numb_days > 0))
    
  es_data2 <- es_data |> 
    filter(numb_days > 0, year > 2019) |> 
    group_by(PROVINCE, DISTRICT_ADM02_NAME, SITE_NAME, year, Lat_Y) |> 
    summarise(median_days = median(numb_days), max_days = max(numb_days) , .groups = "drop") 
  
 problem_sites <- es_data2 |> 
   filter(year == 2020) |> 
    arrange(-max_days) |> 
   head(30) |> pull(SITE_NAME)
 
 es_data2 |> 
   filter(SITE_NAME %in% problem_sites) |> 
   ggplot() + 
   geom_tile(aes(x = year, y = SITE_NAME, fill = median_days), color = "grey") + 
   scale_fill_distiller(palette = "Reds", direction = 1) + 
   facet_wrap(~PROVINCE, scales = "free") + 
   theme_classic()
    
 
 es_data2 |> 
   ggplot() + 
   geom_tile(aes(x = year, y = fct_reorder(SITE_NAME, Lat_Y), fill = median_days), color = "grey") + 
   scale_fill_distiller(palette = "Reds", direction = 1) + 
   theme_classic()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
  








