

# notes ####
# notes but more ####
## Notes but different ####

# 09/01/2026 ####
getwd()
setwd(User/crbkb/Data_Course_BULLOCK/Data)
read.csv( file = '1620_scores.csv')
obj =read.csv(file = '/Users/crbkb/Data_Course_BULLOCK/Data/1620_scores.csv')
obj
csv_files=list.files(path = '/Users/crbkb/Data_Course_BULLOCK/Data/', pattern = 'csv')
csv_files

length(csv_files)
df=read.csv(file = 'wingspan_vs_mass.csv')
head(df)
tail(df)
b_files=list.files( pattern= "^b", ignore.case = FALSE, recursive = TRUE)

for (i in b_files){
  out = readLines(i, n = 1)
  print(out)
}

for (f in csv_files) {
  out = readLines(f, n = 1)
  print(out)
}

  # important function ####
vector
array
matrix
# allows different values but they have to be in equal number
data frame
dat = data.frame(
  id = c(1,2,3),
  weight = c(4,5,6),
  color = c('red','blue','green')
)

list


arr=array

# Matrices 
mat= matrix( data=c(1,2,3,4,5,6,7,8), nrow = 4, ncol = 4, byrow = TRUE)

dat
mat
str(df)

# this is how to create functoin ####
my_fun = function(x,y){
  out = x +y 
  print(out)
}
my_fun(1,2)

#loop how to make loops ####

i like apple
i like orange
i like banana
i like pear

fruit = c('apple','orange','banana','pear','peach','mango')

for (i in fruit){
  out = paste('I like', i)
  print(out)
}

#try to write a for loop####

animals = c('cats','dogs','bears','mice','lizards','birds')

for (i in animals){
  out = paste ('all', i, 'to heaven')
  print(out)
}

#while-loop: you want something to happen if its true in the contitiono spot, then you write something below it to happen####
while (condition) {
  
}
m=1
while (m < 6) {
  print('cool')
  m = m + 1
}

#09/15/26 ####
df_fruit = data.frame(
  fruit,
  cal=c('1','22','33','44','1009','398')
)
str(df_fruit)
df_fruit$cal = as.numeric(df_fruit$cal)
df_fruit$caloriers_100 = df_fruit$cal+100
df_fruit
df_fruit$color= c('red','orange','yellow','green','peach','yellow-green')

for ( i in 1:nrow(df_fruit)) {
  out= paste(df_fruit$fruit[i], 'calories is', df_fruit$caloriers_100[i])
  print(out)
}
for ( i in 1:nrow(df_fruit)) {
  out= paste(df_fruit$fruit[i], 'color is ', df_fruit$color[i])
  print(out)
}
dim(df_fruit)
write.csv(df_fruit, 'df_fruit.csv')
#this is how to start to analys data sets
data(mtcars)
df_cars=mtcars
dim(df_cars)
str(df_cars)
names(df_cars)
df_cars[1]
View(df_cars)
summary(df_cars$mpg)
avgmpg=(mean(df_cars$mpg))

#this makes a data from of all the cars with higher than average mpg 
df_mpg_20=df_cars[df_cars$mpg > avgmpg,]
#this adds a new condition
df_mpg_4=df_mpg_20[df_mpg_20$cyl == 4,]

# you can also combinde the two into one line
df_mpg_21=df_cars[df_cars$mpg > avgmpg & df_cars$cyl == 4,]

# you can remove columns using - only with numbers you can do the same thing with the !
# to remove rows and columns you can us something like this df_cars[,-1] for columns and for rows df_cars[-1,] if you want to remove specific rows you can use -c(1,2,3)


names(df_cars)
new_cars=df_cars[,-c(4,6)]
names(new_cars)
new_new_cars=new_cars[new_cars$mpg > avgmpg & !(new_cars$cyl == 4),]
new_new_cars

# to use the QR god thing
#save url like this url<- 'the url here' 
#then use the qr <- qr_code(url)
#then plot(qr)
#for using tidyverse you can do the sorting using filter

Newish_cars= df_cars%>%
  select(-hp, -wt)%>%
  dplyr::filter(mpg > avgmpg, cyl == 4)
Newish_cars

#pluck means to take something out vs select keeps the format. us pluck when you want to take the average of some df
Newish_cars= df_cars%>%
  dplyr::filter( !cyl == 4)%>%
  pluck('mpg')%>%
  mean()
Newish_cars






# 09/22/26 ####
library(tidyverse)
cbd=read.csv('Data/cleaned_bird_data.csv')
mean_cbd=mean(cbd$Egg_mass, na.rm=TRUE)
sd_cbd=sd(cbd$Egg_mass, na.rm=TRUE)
large=cbd[cbd$Egg_mass > mean_cbd & !is.na(cbd$Egg_mass),]
dim(large)


large_eggs= cbd%>%
  dplyr::filter(Egg_mass > mean_cbd)

dim(large2)
View(large2)
View(large)

write.csv(large_eggs, 'Large Eggs.csv', row.names = FALSE)

cbd%>%
  dplyr::filter(Egg_mass > mean_cbd)%>%
  summarise(min_egg = min(Egg_mass),
            max_egg = max(Egg_mass),
            avg_egg = mean(Egg_mass),)

# 09/24/26
library(palmerpenguins)
library(tidyverse)

penguins=penguins

avgpen= penguins%>%
  filter(!is.na(bill_length_mm) & !is.na(sex))%>%
  group_by(species, sex)%>%
  summarise(average_bill=mean(bill_length_mm), #the summarise will orginize all of the groups together
            median_bill=median(bill_length_mm),
            IQR_bill=IQR(bill_length_mm))%>%
  arrange(desc(average_bill))%>% #this will change the order of the rows. usually using desc to get the highest numbers at the top
  #if you wanted to change specifically use arrange(match(species,c(then list the stuff)))
  relocate(species, .after=sex)%>%#this is how to move columns 
  mutate(sum_of_mean_median = average_bill + median_bill, .before=average_bill)%>%#this is how you create a new column and you can move it where ever you want
  select(-sum_of_mean_median)%>%#this will remove columns
  mutate(Chungus_or_Nah= case_when(average_bill > mean(average_bill)~ 'big chungus', TRUE ~ 'little chad'))#case when is an if(true) then statment with the condition on the left of the ~
  


  ##############################this stuff is AI         
ggplot(penguins, aes(x = bill_length_mm)) +
  geom_histogram(binwidth = 1, na.rm = TRUE) +
  facet_wrap(~ species) +
  labs(x = "Bill length (mm)", y = "Count")

penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = bill_length_mm)) +
  geom_histogram(binwidth = 2, na.rm = TRUE) +
  facet_grid(sex ~ species) +
  labs(x = "Bill length (mm)", y = "Count")

ggplot(penguins, aes(x = species)) +
  geom_bar() +
  labs(x = "Species", y = "Number of penguins")
  
penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = species, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(x = "Species", y = "Number of penguins")

###############################################

#09/29/26 ####

# find penguins body mass> 5000 and bill length > average
## count how many male and female are on each island
# what are the max weight and max bill length
#add new col to indicate whether the they meet the 

library(palmerpenguins)
library(tidyverse)

penguins=penguins

avgpen= penguins%>%
  filter(!is.na(body_mass_g))%>%
  filter(!is.na(bill_length_mm))%>%
  filter(!is.na(sex))  %>%
  filter(!is.na(island)) %>%
  filter(body_mass_g > 5000 & bill_length_mm > mean(bill_length_mm))%>%
  group_by(sex, island)%>%
  summarise(
    maxL=max(bill_length_mm),
    maxW=max(body_mass_g),
    count=n()
  )
 

agree= penguins%>%
  mutate(criteria = case_when(body_mass_g > 5000 & bill_length_mm > mean(penguins$bill_length_mm, na.rm = T) ~ 'big',
                              TRUE ~ 'small'))%>%
  View()



penguins$body_mass_g > 5000
library(ggplot2)

penguins%>%
  drop_na()%>%
ggplot(aes(x = body_mass_g,
            y = bill_length_mm,
            color=sex
            ))+
        geom_point()+
        geom_smooth(method = 'lm')
        
ggsave()#how work
#10/01/26####

library(palmerpenguins)
library(tidyverse)
library(ggplot2)
p_data=penguins

hist_body_mass=p_data%>%
  drop_na()%>%
  ggplot(aes(
    x= body_mass_g,
    fill = sex,))+
  geom_histogram(alpha = 0.8, position = 'identity', bins = 20)+
  scale_fill_manual(values= c("female"='orchid', "male"='steelblue'))

ggsave('Hist_of_Body_Mass.PDF',plot=hist_body_mass, width=10, height=8, units = 'in',dpi=500 )    


p_data%>%
  drop_na()%>%
  ggplot(aes(x=bill_length_mm,
             fill=island, linetype=island))+
  geom_density(alpha= 0.3, position = 'dodge')+
  scale_linetype_manual(values=c('Biscoe'='dashed','Dream'='solid', 'Torgersen'='dotted'))+
  scale_fill_manual(values= c("Biscoe"='orchid', "Dream"='steelblue','Torgersen'='forestgreen'))

p_data%>%
  drop_na()%>%
  group_by(species)%>%
  summarize(mean_body_mass=mean(body_mass_g),
            sd_body_mass=(sd(body_mass_g)))%>%
  ggplot(aes(x=species,
             y=mean_body_mass,
             fill= species))+
  geom_col(alpha=0.8, position = 'identity')+
  geom_errorbar(aes(ymin = mean_body_mass - sd_body_mass,
                    ymax = mean_body_mass + sd_body_mass),
                width = 0.2) +
  scale_fill_manual(values= c("Adelie"='orchid', "Chinstrap"='steelblue','Gentoo'='forestgreen'))+
  labs(x= 'Species',
       y= 'Mean Body Mass',
       title='Average Body mass of Penguin Species')


p_data%>%
  drop_na()%>%
  ggplot(aes(y=bill_length_mm,
             x=body_mass_g,
             colour = species))+
  geom_point(alpha=0.8)+
  facet_wrap(~sex)+#this will split into one group facet_grid splits into two catigories
  scale_color_manual(values= c("Adelie"='orchid', "Chinstrap"='steelblue','Gentoo'='forestgreen'))+
  theme_minimal()#this is how to change everything else but the data like the back ground and numbers spacing 


#10/06/26####

