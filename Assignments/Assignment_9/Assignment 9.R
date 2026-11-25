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
Grad=read.csv('GradSchool_Admissions.csv')
#1 is admited 0 is rejected

gpa_plot=Grad%>%
  ggplot(aes(x=gpa,
             y=admit))+
  geom_jitter()+
  geom_smooth(method = 'glm', se=F)+
  facet_wrap(~rank)
gpa_plot

gre_plot=Grad%>%
  ggplot(aes(x=gre,
             y=admit))+
  geom_jitter()+
  geom_smooth(method = 'glm', se=F)+
  facet_wrap(~rank)
gre_plot

gre_plot+gpa_plot

Grad%>%
  ggplot(aes(x=rank,
             y=admit))+
  geom_smooth(method = 'lm', se=F)
###Box plots

Grad2=Grad%>%
  mutate(admit=factor(admit,
                      levels=c(0,1),
                      labels=c('Not Admitted', 'Admitted')))%>%
  mutate(rank=factor(rank))



gpa2_plot=Grad2%>%
  ggplot(aes(x=gpa,
             y=admit))+
 geom_boxplot()
gpa2_plot

gre2_plot=Grad2%>%
  ggplot(aes(x=gre,
             y=admit))+
  geom_boxplot()
gre2_plot

(gre_plot+gpa_plot)/(gre2_plot+gpa2_plot)

gpa2_plot=Grad2%>%
  ggplot(aes(x=gpa,
             y=admit))+
  geom_boxplot()
gpa2_plot

gre2_plot=Grad2%>%
  ggplot(aes(x=gre,
             y=admit))+
  geom_boxplot()
gre2_plot
###


###########################################################################



mod1=glm(data=Grad2, admit~gre+gpa+rank, family=binomial)
summary(mod1)

mod2=glm(data=Grad, admit~gre+gpa)
summary(mod2)

mod3=glm(data=Grad, admit~gre+rank)
summary(mod3)

mod4=glm(data=Grad, admit~gpa+rank)
summary(mod4)

mod5=glm(data=Grad, admit~gre)
summary(mod5)

mod6=glm(data=Grad, admit~gpa)
summary(mod6)

mod7=glm(data=Grad, admit~rank)
summary(mod7)


Grad2$prediction=predict(mod1,Grad2)
compare_performance(mod1,mod2,mod3,mod4,mod5,mod6,mod7)%>%
  plot()


gather_predictions(Grad2, mod1)%>%
  ggplot(aes(x=gpa,
             y=prediction))+
  geom_smooth(se=F)

gather_predictions(Grad2, mod1)%>%
  ggplot(aes(x=gre,
             y=prediction))+
  geom_smooth(se=F)

