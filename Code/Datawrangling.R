library(tidyverse)
library(readxl)
getwd()
mytheme=theme(
  panel.background = element_rect(fill = "white", 
                                  colour = "grey50"))
theme_set(mytheme)
raw=read_xlsx("./Data/Raw/Raw_Data.xlsx") %>%
  mutate(Country=as.factor(Country))
summary(raw$Country)
#select 6 contries that has most data in each continent
Six.Countries=raw %>%
  select(c(1:3,5)) %>%
  rename(Index=`Environmental Intensity (Sales)`)%>%
  rename(Company=`Company Name`) %>%
  mutate(Country=as.factor(Country)) %>%
  mutate(Company=as.factor(Company)) %>%
  mutate(Index=Index*(-1))%>%
  mutate(Year=as.factor(Year)) %>%
  filter(Country=="Japan"|Country=="United Kingdom"|
           Country=="United States"|Country=="South Africa"|
           Country=="Australia"|Country=="Mexico")

write.csv(Six.Countries, row.names = FALSE,
          file="./Data/Processed/SixCountries.csv")          
