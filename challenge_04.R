#Question 1: Cumulative Covid-19 cases
#Goal: 
#Map the time course of the cumulative Covid-19 cases! 
#Your plot should look like this


#Los gehts!!!!!
#Lets go



#STEP 1 
#LOAD LIBRARIES


library(tidyverse)
library(lubridate)
library(ggthemes)


#Step 2
#Load Data

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")


#Step 3
#Data Manipulation

#Do not forget to ungroup

cumulative_cases_tbl <- covid_data_tbl %>%
  filter(year == 2020) %>%
  filter(countriesAndTerritories %in% c("Germany", "United_Kingdom", "France", "Spain", "United_States_of_America")) %>%
  filter(month %in% c(1:11)) %>%
  select(month,dateRep, countriesAndTerritories, cases) %>%
  mutate(date = dmy(dateRep)) %>%
  arrange(date) %>%
  group_by(countriesAndTerritories) %>%
  mutate(cum_cases = cumsum(cases)) %>%
  ungroup ()



#Step 4

#Data Visualisation

cumulative_cases_tbl %>%
  #Canvas
  ggplot(aes(x = date, y =cum_cases, color = countriesAndTerritories)) +
  #Geometries
  geom_line(size = 0.5, linetype = 1) +
  
  # geom_label(aes(label = label_txt),
  #         hjust = "inward",
  #           size  = 2) +
  # 
  #Formatting
  expand_limits(y = 0.0) +
  scale_x_date(breaks = "1 month", date_labels = "%B") +
  # scale_x_continuous(breaks = cumulative_cases_tbl$month,
  #                      labels = month(cumulative_cases_tbl$date, label = T)) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6,
                                                    big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "",
                                                    suffix = "M")) +
  # breaks = seq(0, 000000, by =8)) +
  labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = "As of 11/02/2020, Europe had more cases than the USA",
    x = "Year 2020",
    y = "Cumulative Cases",
    color = "Country")  +  
  theme_bw() +
  theme(legend.position  = "bottom", 
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45),
        plot.title = element_text(face = "bold")
  )




#ON TO THE NEXT ONE
#Question 2

# Goal 
#Visualize the distribution of the mortality rate (deaths / population) with
# geom_map().


#Additional librarz
library(maps)


world <- map_data("world")

covid_mortality_rate_tbl  <- covid_data_tbl %>%
  select(countriesAndTerritories, deaths, popData2019, continentExp) %>%
  group_by(countriesAndTerritories, popData2019)%>%
  summarise(sum_deaths_country = sum(deaths)) %>%
  mutate(mortality_rate = sum_deaths_country / popData2019) %>%
  ungroup() %>%
  
  
  
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
    
  ))


Map_tbl <- covid_mortality_rate_tbl %>%
  left_join(world, by = c( "countriesAndTerritories" = "region")) %>%
  rename(region = countriesAndTerritories)


#Data Visualization
Map_tbl %>%
  ggplot() +
  geom_map(aes(map_id = region, fill = mortality_rate ), map = world ) +
  expand_limits(x= Map_tbl$long, y =Map_tbl$lat) +
  scale_fill_gradient(low = "#ee4540", high = "#2d142c", labels = scales::percent) +
  
  labs(
    title = "Confirmed COVID-19 deaths relative to the size of the population",
    subtitle = "More than 1.2 Million confirmed COVID-19 deaths worldwide",
    x = " ",
    y = " ",
    color = "Mortality Rate")  +  
  theme_dark() +
  theme(legend.position  = "right", 
        legend.direction = "vertical",
        plot.title = element_text(face = "bold"),
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank()
  )









