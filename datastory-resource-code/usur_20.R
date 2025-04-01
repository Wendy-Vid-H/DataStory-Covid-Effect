library(tidyverse)
library(tigris)
library(tidycensus)
library(censusapi)
library(sf)

### Import the adjusted monthly male unemployment rate from 2017 to 2021 ###
library(readxl)
mur_adj20 <- read_excel("2020_men_unemployment.xlsx", 
                        sheet = "madj")

m_timeseq <- c("Jan", "Feb", "Mar", "Apr", "May", 
                 "Jun", "Jul", "Aug", "Sep", 
                 "Oct", "Nov", "Dec")

mur_adj20 <- mur_adj20 |>
  mutate(monf = factor(Month, levels = m_timeseq))

### Heatmap for Monthly Male Unemployment Rate from 2017 to 2021 (not included in final data story) ###
mur_adj20 |>
  ggplot(mapping = aes( x = Year,
                        y = fct_rev(monf))) +
  geom_tile(aes(fill = mur_adj))+
  scale_fill_gradient(low = "aliceblue",
                      high = "dodgerblue4")+
  labs(fill = "Unemployment \n Rate (%)",
       title = "Monthly Male Unemployment Rate, from 2017 to 2021",
       caption = "Data Source: BLS data, adjusted")

### Import the adjusted monthly female unemployment rate from 2017 to 2021 ###
library(readxl)
wur_adj20 <- read_excel("2020_women_unemployment.xlsx", 
                        sheet = "wadj")

wur_adj20 <- wur_adj20 |>
  mutate(monf2 = factor(Month, levels = m_timeseq))

### Heatmap for Monthly Female Unemployment Rate from 2017 to 2021 (not included in final data story) ###
wur_adj20 |>
  ggplot(mapping = aes( x = Year,
                        y = fct_rev(monf2))) +
  geom_tile(aes(fill = wur_adj))+
  scale_fill_gradient(low = "azure",
                      high = "aquamarine4") + 
  labs(fill = "Unemployment \n Rate (%)",
       title = "Monthly Female Unemployment Rate, from 2017 to 2021",
       caption = "Data Source: BLS data, adjusted")

### Merge the 2 dataset and calculate the gender gap in unemployment rate ###

tur_agj20 <- mur_adj20 |>
  left_join(wur_adj20, by = c("Year", "Month"))

tur_adj20 <- tur_agj20 |>
  mutate(gap_ur = wur_adj - mur_adj)


### Heatmap for the gender-gap in monthly unemployment rate from 2017 to 2021 ###
tur_adj20 |>
  ggplot(mapping = aes( x = Year,
                        y = fct_rev(monf2))) +
  geom_tile(aes(fill = gap_ur))+
  scale_fill_gradient2(low = "violetred",
                       mid = "grey99",
                      high = "darkgreen",
                      midpoint = 0,
                      limits = c(-1.0, 3.0))+
  labs(title = "Women Faced a Higher Unemployment Rate During the Covid Period",
       subtitle = "This was very different from the situation in previous years",
       caption = "Source: Bureau of Labor Statistics, \n(Not Seasonally Adjusted)",
       x = "Year",
       y = "Month") +
  theme_minimal()
