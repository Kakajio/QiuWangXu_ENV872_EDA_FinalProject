library(tidyverse)
library(readxl)
getwd()
raw=read_xlsx("./Data/Raw/Raw_Data.xlsx")
class(raw)
summary(raw$Country)
#select 5 contries that has most data in each continent
Five.Countries=raw %>%
  select(c(1:3,5)) %>%
  rename(Index=`Environmental Intensity (Sales)`)%>%
  rename(Company=`Company Name`) %>%
  mutate(Country=as.factor(Country)) %>%
  mutate(Company=as.factor(Company)) %>%
  mutate(Index=Index*(-1))%>%
  mutate(Year=as.factor(Year)) %>%
  filter(Country=="Japan"|Country=="United Kingdom"|
           Country=="United States"|Country=="South Africa"|
           Country=="Australia")
dim(Five.Countries)
summary(Five.Countries)
#Compare average annual environmental impact level by countries
Annual.average.line=Five.Countries %>%
  group_by(Country,Year)%>%
  summarize(m=mean(Index))%>%
  ggplot(aes(Year,
             m,
             color=Country,group=Country))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)
print(Annual.average.line)  
#Compare companies' annual environmental impact level distribution by countries
Annual.average.box=Five.Countries %>%
  group_by(Country,Year)%>%
  ggplot(aes(x=Index,y="",
             color=Country))+
  geom_boxplot()+
  facet_wrap(~Year)
print(Annual.average.box)
write.csv(Five.Countries, row.names = FALSE,
          file="./Data/Processed/FiveCountries.csv")
          
