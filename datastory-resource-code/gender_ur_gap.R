library(tidyverse)
library(tigris)
library(tidycensus)
library(censusapi)
library(sf)

library(hrbrthemes)

##### Graph 2: Unemployment Rate by Marital Status ###############################
### Data: Unemployment Rate for people with different marital status, from BLS ###

library(readxl)
gapstlo <- read_excel("BLS data/gender_marriage.xlsx", 
                      sheet = "gapstlo")

m_timeseq <- c("Jan", "Feb", "Mar", "Apr", "May", 
               "Jun", "Jul", "Aug", "Sep", 
               "Oct", "Nov", "Dec")
gapstlo <- gapstlo |>
  mutate(monf = factor(Year, levels = m_timeseq))

gapstlo |>
  ggplot(mapping = aes(x = monf,
                       y = gap)) +
  geom_col(mapping = aes(fill = ma_status),
           position = "dodge") +
  scale_fill_manual(values = c("seperated" = "#FED789FF",
                               "married" = "#023743FF",
                               "never" = "#72874EFF"),
                    labels = c("seperated" = "Seperated, Widowed,\n Divorced",
                               "married" = "Married,Spouse \n Present",
                               "never" = "Never Married")) +
  labs(title = "Marriage made women much more likely to lose jobs compared to their husbands",
       subtitle = "The gap between the unemployment rate for married men and women were the largest during the Covid",
       caption = "Source: Bureau of Labor Statistics, \n(Not Seasonally Adjusted)",
       x = "Month, 2020",
       y = "Unemployment Rate Gap(%) \n (Women - Men)")+
  theme_minimal()

### A further explore (not included in the data story) ###
### Add the change of gender gap in monthly unemployment rate into the plot ###
gapstlo |>
  ggplot() +
  geom_col(mapping = aes(x = monf,
                         y = gap,
                         fill = ma_status),
           position = "dodge") +
  geom_line(mapping = aes(x = monf,
                          y = wur,
                          linetype = ma_status,
                          group = ma_status),
            color = "cornflowerblue",
            linewidth = 0.8) +
  scale_fill_manual(values = c("seperated" = "#FED789FF",
                               "married" = "#023743FF",
                               "never" = "#72874EFF"),
                    labels = c("seperated" = "Seperated, Widowed,\n Divorced",
                               "married" = "Married,Spouse \n Present",
                               "never" = "Never Married"))+
  geom_point(mapping = aes(x = monf,
                           y = wur),
             color = "darkorchid") +
  labs(title = "Marriage made women much more likely to lose jobs compared to their husbands",
       subtitle = "Despite that the never-married women faced the highest unemployment rate,\nthe gap between the unemployment rate for married men and women were the largest during the Covid",
       caption = "Source: Bureau of Labor Statistics, \n(Not Seasonally Adjusted)",
       x = "Month, 2020",
       y = "Unemployment Rate Gap \n (Women - Men)")


##### Graph 3: Gender Unemployment Rate Gap: by Races & Ethnics ######################
### Data: Unemployment Rate for men and women of different races/ethnics, from BLS ###
### 3 groups here: white, black, Hispanic & Latino ###################################

library(readxl)
race20 <- read_excel("BLS data/gender_race/race20.xlsx")


race20 <- race20 |>
  mutate(monf = factor(Year, levels = m_timeseq)) |>
  mutate(month = fct_rev(monf))

  race20 |>
    ggplot() +
    geom_segment(aes(x = month, xend = month,
                     y = bl_m, yend = bl_w),
                 color = "plum") +
    geom_point( aes(x=month, y = bl_m,
                    color = "Black/African American Men"), size=3,alpha = 0.7 ) +
    geom_point( aes(x=month, y = bl_w,
                    color = "Black/African American Women"), size=3,alpha = 0.7 ) +
    geom_segment(aes(x = month, xend = month,
                     y = wh_m, yend = wh_w),
                 color = "cornflowerblue") +
    geom_point( aes(x=month, y = wh_m,
                    color = "White Men"), size=3,alpha = 0.7 ) +
    geom_point( aes(x=month, y = wh_w,
                    color = "White Women"), size=3,alpha = 0.7) +
    geom_segment(aes(x = month, xend = month,
                     y = hisp_m, yend = hisp_w),
                 color = "seagreen1") +
    geom_point( aes(x=month, y = hisp_m,
                    color = "Hispanic or Latino Men"), size= 3,alpha = 0.7) +
    geom_point( aes(x=month, y = hisp_w,
                    color = "Hispanic or Latino Women"), size= 3,alpha = 0.7) +
    scale_color_manual(labels = c("Black/African American Men" ="Black/African\nAmerican Men" ,
                                  "Black/African American Women" = "Black/African\nAmerican Women",
                                  "White Men" = "White Men",
                                  "White Women" = "White Women",
                                  "Hispanic or Latino Men" = "Hispanic or\nLatino Men",
                                  "Hispanic or Latino Women" = "Hispanic or\nLatino Women"),
                       values = c("Black/African American Men" ="#855C75FF",
                                  "Black/African American Women" = "#736F4CFF",
                                  "White Men" = "#D9AF6BFF",
                                  "White Women" = "grey",
                                  "Hispanic or Latino Men" = "#AF6458FF",
                                  "Hispanic or Latino Women" = "#467378FF"))+
    coord_flip() +
    theme_minimal() +
    labs(title = "Inequality in Race Aggravated also during the Pandemic",
         subtitle = "The differences of unemployment rate between men and women were different within each race,\nhowever, the sparity between different races or ethincs was significant",
         caption = "Source: Bureau of Labor Statistics, \n(Not Seasonally Adjusted)",
         x = "Month, 2020",
         y = "Unemployment rate (%)")
