# Data Science at TUHH ------------------------------------------------------

# Chapter 5 ----

# Load libraries ----
library(tidyverse)
library(lubridate)
library(maps)

# DATA IMPORT ----

col_types_covid <- list(
  dateRep = col_date("%d/%m/%Y"),
  day = col_double(),
  month = col_double(),
  year = col_double(),
  cases = col_double(),
  deaths = col_double(),
  countriesAndTerritories = col_character(),
  geoId = col_character(),
  countryterritoryCode = col_character(),
  popData2019 = col_double(),
  continentExp = col_character(),
  `Cumulative_number_for_14_days_of_COVID-19_cases_per_100000` = col_double()
)

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", col_types = col_types_covid)

# Ch.1 Plot ----

plot_1_tbl <- covid_data_tbl %>% select(dateRep,countriesAndTerritories,cases) %>% arrange(dateRep) %>%
              group_by(dateRep) %>% summarise(Total_cases = sum(cases))%>%
 arrange(dateRep)

cumsum <- plot_1_tbl %>% mutate(cumsum = cumsum(Total_cases))

cumsum %>%
  
  ggplot(aes(dateRep, cumsum)) +
  
  theme_light() +
  theme(
    
    title = element_text(face = "bold", color = "#08306B")
  ) +
  scale_x_date(limits = as.Date(c("2019-12-31","2020-12-06"))) +
  ggtitle("limits = as.Date(c(\"2019-12-31\",\"2020-12-06\"))")+
  
  scale_y_continuous(labels = scales::scientific_format(scale = 1e-6, 
                                                        preix = "*",
                                                        suffix = "")) +
  
  labs(
    title = "Covid-19 Cases",
    subtitle = "Number of cases trending up",
    caption = "",
    x = "Date",
    y = "Cases",
    color = "cumsum" # Legend text
  )+
  
  geom_line(size = 0.8, linetype = 1 , color = "blue")

# Ch.2 Plot ----

world_map <- map_data("world")


mor_rate_by_country <- covid_data_tbl %>% group_by(countriesAndTerritories)%>% summarise(Mor_rate = sum(deaths)/sum(popData2019))


mor_rate_by_country <- mor_rate_by_country %>% mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
    
  ))
test <- world_map %>% left_join(mor_rate_by_country, by = c("region" = "countriesAndTerritories"))

test <- test %>% mutate(More_rate_factored = Mor_rate*10000000000)
test <- test %>% filter(Mor_rate > 0) 
ggplot(test, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=More_rate_factored), colour = "white") +
  expand_limits(x = test$long, y = test$lat)+
  scale_fill_viridis_c(option = "C")+
  labs(
    title = "World Wide Mortality Rate",
    subtitle = "",
    fill = "Mortality Rate" # Changes the legend name
  )