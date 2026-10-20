library(ggplot2)
library(tidyverse)

# Q1
clean_data=read.csv('cleaned_covid_data.csv')
View(clean_data)

#Q2
?grepl

A_States=clean_data%>%
  filter(str_starts(Province_State, 'A'))


## Q3 Plot
clean_data%>%
  drop_na()%>%
  ggplot(aes(x=Last_Update,
             y=Deaths))+
  geom_point()+
  geom_smooth(method="loess", se=F)+
  facet_wrap(~Province_State, scales= 'free')+
  labs(x= 'Time',
       y= 'Deaths')

#Q4
state_max_fatality_rate=clean_data%>%
  drop_na()%>%
group_by(Province_State)%>%
  summarize(Maximum_Fatality_Ratio=max(Case_Fatality_Ratio))%>%
 arrange(desc(Maximum_Fatality_Ratio))


#Q5
state_max_fatality_rate%>%
  ggplot(aes(x= fct_reorder(Province_State, Maximum_Fatality_Ratio, .desc = T),
             y=Maximum_Fatality_Ratio))+
  geom_col()+
  labs(x='State',
       y= 'Maximum Fatality Rato')+
  theme(axis.title.y =element_text(angle=0
                                   , vjust = 0.5), axis.text.x = element_text(angle=45) )
