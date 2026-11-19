library(readxl) 
library(gapminder)
library(gganimate)
library(gifski)
library(av)
library(palmerpenguins)
library(tidyverse)
library(ggplot2)
library(ggimage)
library(patchwork)
library(ggmap)
library(leaflet)
library(extrafont)
library(janitor)
library(skimr)
library(easystats)
setwd('..')
UTBC=read.csv('Utah_Religions_by_County.csv')
Finished=UTBC%>%
  drop_na()%>%
  mutate(County=str_trim(str_remove(County, ' County')))%>%
  pivot_longer(cols = -c(County,Pop_2010,Non.Religious,Religious,), names_to = 'Religions', values_to = 'Proportions_Religion')%>%
  select(-Non.Religious)
#I had to get rid of one of the religous or non religous proportions because they kept getting added together when I would make the graphs. 
#other than that just making the data look nice

Finished%>%
  ggplot(aes(x=Pop_2010,
             y=Religious
             ))+
  geom_smooth(method = 'glm', se=F)+
  facet_wrap(~Religions)
#This first graph shows that as population increases that so does the proportion of religious people.
Finished%>%
  ggplot(aes(x=Religious,
             y=Proportions_Religion))+
  geom_smooth(method='glm', se=F)+
  facet_wrap(~Religions)
#For this second one, there does seem to appear to be a pretty strong corilation with how many people are religious and if they are the lds religion.
# it seems to be a positive relationship that if they are religious they will more likely be LDS
