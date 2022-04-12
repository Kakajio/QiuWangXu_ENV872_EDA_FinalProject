library(tidyverse)
library(readxl)
getwd()
mytheme=theme(
  panel.background = element_rect(fill = "white", 
                                  colour = "grey50"))
theme_set(mytheme)
raw=read.csv("./Data/Processed/FiveCountries.csv",stringsAsFactors = TRUE)

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
  geom_smooth(method="lm",se=FALSE)+
  labs(x = "Year", y = "Impact Intensity")  +
  ggtitle("Annual Average Company Environmental Impact Intensity")
print(Annual.average.line)  
#Compare companies' annual environmental impact level distribution by countries
Annual.average.box=Five.Countries %>%
  group_by(Country,Year)%>%
  ggplot(aes(x =Index,y="",
             color=Country))+
  geom_boxplot()+
  facet_wrap(~Year)+
  labs(x = "Impact Intensity", y = "Country")  +
  ggtitle("Annual Company Environmental Impact Intensity Distribution")
print(Annual.average.box)
write.csv(Five.Countries, row.names = FALSE,
          file="./Data/Processed/FiveCountries.csv")