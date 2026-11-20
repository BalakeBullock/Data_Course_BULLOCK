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
library(modelr)
library(broom)
library(fitdistrplus)

 setwd('../../Data')

mush=read.csv('mushroom_growth.csv')
view(mush)
#################################################
#Q2
Temp=mush%>%
  ggplot(aes(x=Temperature,
             y=GrowthRate,))+
  geom_boxplot()+
  facet_wrap(~Species)
  
Light=mush%>%
  ggplot(aes(x=Light,
             y=GrowthRate,))+
  geom_boxplot()+
  facet_wrap(~Species)  
  
Hum=mush%>%
  ggplot(aes(x=Humidity,
             y=GrowthRate,))+
  geom_boxplot()+
  facet_wrap(~Species)  

Nitrogen=mush%>%
  ggplot(aes(x=Nitrogen,
             y=GrowthRate,))+
  geom_boxplot()+
  facet_wrap(~Species)  
combined_plot=(Hum+Light)/(Nitrogen+Temp)
combined_plot

#####################################################
#Q3/Q4
mod1=glm(data=mush, GrowthRate~Temperature)
summary(mod1)
MSE1=mean(mod1$residuals^2)

mod2=glm(data=mush, GrowthRate~Nitrogen)
summary(mod2)
MSE2=mean(mod2$residuals^2)

mod3=glm(data=mush, GrowthRate~Humidity)
summary(mod3)
MSE3=mean(mod3$residuals^2)

mod4=glm(data=mush, GrowthRate~Light)
summary(mod4)
MSE4=mean(mod4$residuals^2)

mod5=glm(data=mush, GrowthRate~Temperature+Light+Humidity+Nitrogen)
summary(mod5)
MSE5=mean(mod5$residuals^2)

mod6=glm(data=mush, GrowthRate~Species+Temperature+Light+Humidity+Nitrogen)
summary(mod6)
MSE6=mean(mod6$residuals^2)
compare_performance(mod1,mod2,mod3,mod4)
plot(compare_performance(mod1,mod2,mod3,mod4,mod5,mod6))
###################################################
#Q5
#I think that model 6 is the best one based off of the MSE values and the compared preformance values.

#########################################
#Q6

mush$pred=predict(mod6,mush)

mush_long = mush %>%
  pivot_longer(cols = c(GrowthRate, pred),
               names_to = "Type",
               values_to = "Value") %>%
  mutate(Type = recode(Type, GrowthRate = "Actual", pred = "Predicted"))

mush_long%>%
ggplot(aes(x = Humidity, 
           y = Value, 
           fill = Type)) +
  geom_boxplot() +
  facet_wrap(~Species) +
  labs(y = "GrowthRate")
  ###################################################

