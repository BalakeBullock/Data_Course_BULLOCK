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
U5MR=read.csv('unicef-u5mr.csv')

##########################################################################
#cleaning data
U5MR_clean=U5MR%>%
  pivot_longer(cols = starts_with('U5MR.'), names_to = 'Year', values_to = 'U5MR', values_drop_na = T)%>%
  mutate(Year=as.integer(str_trim(str_remove(Year,'U5MR.'))))

###########################################################################
#first plot
plot1=U5MR_clean%>%
  ggplot(aes(x=Year,
             y=U5MR,
             group=CountryName))+
  geom_line()+
  facet_wrap(~Continent)

ggsave('BULLOCK_Plot_1.png', plot1 )


###############################################################################
#second plot
plot2=U5MR_clean%>%
  group_by(Year,Continent)%>%
  summarize(Mean_U5MR=(mean(U5MR)))%>%
  ggplot(aes(x=Year,
             y=Mean_U5MR,
             color=Continent))+
  geom_line(linewidth = 1.5)
  
ggsave('BULLOCK_Plot_2.png', plot2 )

###############################################################################
#creating three models
mod1=glm(data=U5MR_clean, formula = U5MR~Year)
mod2=glm(data=U5MR_clean, formula = U5MR~Year+Continent)
mod3=glm(data=U5MR_clean, formula = U5MR~Year*Continent)


###############################################################################
#compare three models
compare_models(mod1,mod2,mod3)
compmods=compare_performance(mod1,mod2,mod3)
plot(compmods)
#Based off of the plot we can tell that model 3 is the best fit of all the models.


#################################################################################################
#plotting the models
U5MR_clean2=U5MR_clean

U5MR_clean2$pred1=predict(mod1,U5MR_clean)
U5MR_clean2$pred2=predict(mod2,U5MR_clean)
U5MR_clean2$pred3=predict(mod3,U5MR_clean)




plot3=U5MR_clean2%>%
  pivot_longer(cols = c(pred1,pred2,pred3), names_to = 'Model', values_to ='Predictions' )%>%
  ggplot(aes(x=Year,
               y=Predictions,
             color=Continent))+
  geom_line(linewidth = 1.5)+
  facet_wrap(~Model, labeller=as_labeller(c(
    'pred1'='mod1',
    'pred2'= 'mod2',
    'pred3'='mod3'
  )))
ggsave("BULLOCk_plot_3.png", plot3)

