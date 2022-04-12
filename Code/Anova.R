library(tidyverse)
library(agricolae)
mytheme=theme(
  panel.background = element_rect(fill = "white", 
                                  colour = "grey50"))
theme_set(mytheme)
FiveCountries=read.csv("./Data/Processed/FiveCountries.csv",stringsAsFactors = TRUE)
# Test for normality
Year2019=filter(FiveCountries,Year==2019)

shapiro.test(Year2019$Index
             [Year2019$Country=="Australia"])
shapiro.test(Year2019$Index
             [Year2019$Country=="Japan"])
shapiro.test(Year2019$Index
             [Year2019$Country=="South Africa"])
shapiro.test(Year2019$Index
             [Year2019$Country=="United Kingdom"])
shapiro.test(Year2019$Index
             [Year2019$Country=="United States"])
##results: reject null in all 
qqnorm(Year2019$Index);qqline(Year2019$Index)

bartlett.test(Year2019$Index ~ Year2019$Country)
#results: reject null i.e. variances are not equal
Year2019.anova=aov(data = Year2019, Index~Country)
summary(Year2019.anova)
#results: reject null hypothesis 
#i.e. difference between a pair of group means is statiscally significant

group.2019=HSD.test(Year2019.anova, "Country", group = TRUE)
group.2019
group.2019.plot <- ggplot(Year2019, 
                          aes(x = reorder(Country,-Index), y = Index)) +
  geom_boxplot() +
  stat_summary(geom = "text", fun = max, vjust = -1, size = 3.5,
               label = c("a","a","a","b", "b")) +
  labs(x = "Country", y = "Impact Intensity")  +
  ggtitle("2019 Average Company Environmental Impact Intensity")
print(group.2019.plot)
