setwd('..')
getwd()
setwd('..')
getwd()
BPD=read.csv("Data/BioLog_Plate_Data.csv")
View(BPD)

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




plot1=BPD%>%
  mutate(Type=case_when(Sample.ID=='Clear_Creek'~ 'Water',
                          Sample.ID=='Soil_1'~'Soil',
                          Sample.ID=='Soil_2'~'Soil',
                          Sample.ID=='Waste_Water'~'Water'))%>%
  pivot_longer(cols=starts_with('HR'),
               names_to = 'Time',
               values_to = 'Absorbance')%>%
  filter(Dilution==0.1)%>%
  mutate(Time=as.integer(str_remove(Time,'Hr_')))%>%
  ggplot(aes(x=Time,
              y=Absorbance,
             color=Type))+
  geom_smooth()+
  facet_wrap(~Substrate, scales= 'fixed')+
  coord_cartesian(ylim=c(0,2))+
  theme_minimal()+
  labs(x='Time',
       y='Absorbance',
        title='Just Dilution 0.1')
plot1




plot2=BPD%>%
  mutate(Type=case_when(Sample.ID=='Clear_Creek'~ 'Water',
                        Sample.ID=='Soil_1'~'Soil',
                        Sample.ID=='Soil_2'~'Soil',
                        Sample.ID=='Waste_Water'~'Water'))%>%
  pivot_longer(cols=starts_with('HR'),
               names_to = 'Time',
               values_to = 'Absorbance')%>%
  mutate(Time=as.integer(str_remove(Time,'Hr_')))%>%
  filter(str_trim(Substrate)=='Itaconic Acid')%>%
  group_by(Dilution,Sample.ID,Substrate,Time)%>%
  summarize(mean_abs=mean(Absorbance))%>%
 ggplot(aes(x=Time,
             y=mean_abs,
             color=Sample.ID))+
  geom_line()+
  facet_wrap(~Dilution)+
  transition_reveal(Time)+
  theme_minimal()+
  labs(x='Time',
       y= 'Average Absorbance')
  
plot2
