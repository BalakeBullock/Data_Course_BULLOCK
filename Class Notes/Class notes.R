

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
library(GGally)
p_data=penguins

hist_body_mass=p_data%>%
  drop_na()%>%
  ggplot(aes(
    x= body_mass_g,
    fill = sex,))+
  geom_histogram(alpha = 0.8, position = 'identity', bins = 20)+
  scale_fill_manual(values= c("female"='orchid', "male"='steelblue'))

ggsave('Hist_of_Body_Mass.PDF',plot=hist_body_mass, width=10, height=8, units = 'in',dpi=500 )    


p3=p_data%>%
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

library(palmerpenguins)
library(tidyverse)
library(ggplot2)
library(ggimage)
library(patchwork)
library(ggmap)
library(leaflet)
p_data=penguins

p1=p_data%>%
  drop_na()%>%
  ggplot(aes(y=flipper_length_mm,
         x=island, 
         color=island,
         fill=island))+
  geom_violin(alpha=0.3 )+
  geom_jitter( color='chartreuse4')+
  geom_boxplot(alpha=0.5)+
  scale_fill_manual(values= c("Biscoe"='orchid', "Dream"='steelblue','Torgersen'='palegreen'))+
  scale_color_manual(values= c("Biscoe"='black', "Dream"='black','Torgersen'='black'))+
  labs(title = 'Flipper Length of Penguins on Different Islands',
       x="Island",
       y='Flipper Length (mm)')+
  theme(plot.title = element_text(hjust = 0.5))+#this goes from 0-0.5-1
  theme(axis.title.x = element_text(angle = 180, size=25, face = 'bold'))

p2=p_data%>%
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

p3=p_data%>%
  drop_na()%>%
  ggplot(aes(x=bill_length_mm,
             fill=island, linetype=island))+
  geom_density(alpha= 0.3, position = 'dodge')+
  scale_linetype_manual(values=c('Biscoe'='dashed','Dream'='solid', 'Torgersen'='dotted'))+
  scale_fill_manual(values= c("Biscoe"='orchid', "Dream"='steelblue','Torgersen'='forestgreen'))

#this is patch work
p1+p2+p3
combined_plot=(p1+p2)/p3 +
  plot_annotation(
    title ='Name',
    subtitle='Other Name',
      caption='This is pretty cool',
      tag_levels = 'a','b','c'
  )
combined_plot


10/6/2026
Sys.time()#this is the standard for most computer 
dat=as.Date('10/6/2026',format = '%m/%d/%Y')#capital y is for 4 digit year and lower case y is for 2 digit years



DD=read.delim('Data/DatasaurusDozen.tsv')
DD%>%
  drop_na()%>%
  ggplot(aes())

#test#####################################################

p1=p_data%>%
  drop_na()%>%
  ggplot(aes(y=flipper_length_mm,
             x=island, 
             color=island,
             fill=island))+
  geom_violin(alpha=0.3 )+
  geom_image(image = 'Graphics/stunning-penguin-line-art-perfect-for-your-creative-projects-vector.jpg',
             position = position_jitter(width = 0.15, seed = 1),
             size = 0.04)+
  geom_boxplot(alpha=0.5)+
  scale_fill_manual(values= c("Biscoe"='orchid', "Dream"='steelblue','Torgersen'='palegreen'))+
  scale_color_manual(values= c("Biscoe"='black', "Dream"='black','Torgersen'='black'))+
  labs(title = 'Flipper Length of Penguins on Different Islands',
       x="Island",
       y='Flipper Length (mm)')+
  theme(plot.title = element_text(hjust = 0.5))+#this goes from 0-0.5-1
  theme(axis.title.x = element_text(angle = 180, size=25, face = 'bold'))
p1  


#10/08/26####

p_data%>%
  drop_na()%>%
  group_by(sex)%>%
  group 
  ggplot(aes(x=species,
             y=mean(body_mass_g))+
  geom_col()
  
  
p_data%>%
  drop_na()%>%
  ggplot(aes(x=body_mass_g,
             y=bill_length_mm))


DD=read.delim('Data/DatasaurusDozen.tsv')

view(DD)
unique(DD$dataset)

DD%>%
  drop_na()%>%
  group_by(dataset)%>%
  summarize(mean_x=mean(x),
            mean_y=mean(y),
            sd_x=sd(x),
            sd_y=sd(y),)
DD%>%
  drop_na()%>%
  group_by(dataset)%>%
  ggplot(aes(x=x,
             y=y))+
  geom_point()+
  facet_wrap(~dataset)
p_data%>%
  drop_na()%>%
 ggpairs()
 

p_data%>%
  drop_na()%>%
  group_by((sex))%>%
  ggplot(aes(x=bill_depth_mm,
             y=body_mass_g,
             color=sex))+
  geom_point(alpha= 0.8, size = 4)+
  facet_wrap(~species, scales = 'fixed')+
  labs(x='Bill depth (mm)',
       y= ' Body dass (g)',
       color='Sex')+
  theme_minimal()+
  theme(axis.title = element_text(face='bold'),
        axis.text = element_text(face='bold'),
        legend.text = element_text(face = 'bold'),
        legend.title = element_text(face = 'bold'),
        strip.text= element_text(face='bold'),
        panel.border = element_rect(color='darkgrey',linewidth = 1, fill=NA)
          )+
  scale_color_manual(values = c('male'='chartreuse','female'='mediumpurple4'))

#10/13/26####

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
font_import(prompt = FALSE)
loadfonts(device = "win")

p_data=penguins

gap=gapminder

gap%>%
  drop_na()%>%
  group_by(continent)%>%
  summarize(avglife=mean(lifeExp))%>%
  ggplot(aes(x=continent,
             y=avglife,
             fill=continent))+
  geom_col()+
  theme_minimal()+
  labs(title = 'Average life expectancy of each continent',
       x = "Continent",
       y= 'Average Life Expectancy',
       fill='Continent')+
  theme(plot.title=element_text(hjust = 0.5))

gap%>%
  drop_na()%>%
  ggplot(aes(x=pop,
             y=lifeExp,
             color=continent))+
  geom_point()+
  theme_minimal()+
  labs(title = 'Average life expectancy of each continent',
       x = "Continent",
       y= 'Average Life Expectancy',
       fill='Continent')+
  theme(plot.title=element_text(hjust = 0.5))


unique(gap$country)continentunique(gap$country)  
unique(gap$continent)


gap%>%
  ggplot(aes(x=lifeExp,
             y=gdpPercap,
             color =continent))+
  geom_point(aes(size=pop))+
  facet_wrap(~continent)+
  labs(title='Year:{frame_time}')+
  transition_time(time=year)

gap%>%
  ggplot(aes(x=lifeExp,
             y=gdpPercap,
             color =continent))+
    geom_point(aes(size=pop))+
    facet_wrap(~continent)+
    labs(title='Year:{frame_time}')+
    transition_time(time=year)+
    geom_text(aes(label = country), vjust=-1.5, size=1.5, colour = 'black')

anim_save('my first animation.gif',animation=animation)
?transition_time

Dino=read.csv('Assignments/Assignment_4/Dinosaur data for final.csv')
view(Dino)

unique(Dino$lived_in)
Dino_graph=Dino%>%
  drop_na()%>%
  ggplot(aes(x=type,
             fill=lived_in))+
  geom_bar(position = 'dodge')+
  labs(title= 'Number of Dinosaus of a Specific Type',
       x= 'Type',
       y= "Number of Dinosuars",
       fill= 'Country')+
  theme(plot.title = element_text(hjust = 0.5))

ggsave('Assignments/Assignment_4/Example Plot For Assignment 4.PDF',plot=Dino_graph )

#10/20/26####

wideboy=read.csv('Data/wide_income_rent.csv')  
view(wideboy)
good_wide_boy%>%
  ggplot(aes(x=v1,
             y=v2))
good_wide_boy=t(wideboy)


##how to transpose stuff

df= data.frame(
  id=c(1,2,3,4,5),
  height=c(120,130,145,155,170),
  weight=c(34,55,70,80,100)
)


df%>%
   pivot_longer(cols=c(height, weight), names_to = 'type', values_to = 'measurement')
wideboy%>%
  pivot_longer(cols ="Alabama","Alaska",             
                "Arizona","Arkansas", "California"          
               "Colorado", "Connecticut","Delaware"            
               "District.of.Columbia","Florida",:"Georgia"             
               "Hawaii","Idaho","Illinois"            
               "Indiana", "Iowa", "Kansas"              
               "Kentucky"     ,        "Louisiana"        ,    "Maine"        ,       
               "Maryland"      ,       "Massachusetts"    ,    "Michigan"      ,      
               "Minnesota"      ,      "Mississippi"       ,   "Missouri"       ,     
               "Montana"         ,     "Nebraska"          ,   "Nevada"          ,    
               "New.Hampshire"    ,    "New.Jersey"        ,   "New.Mexico"     ,     
               "New.York"          ,   "North.Carolina"    ,   "North.Dakota"    ,    
               "Ohio"               ,  "Oklahoma"         ,    "Oregon"           ,   
               "Pennsylvania"        , "Rhode.Island"    ,     "South.Carolina"   ,   
               "South.Dakota"        , "Tennessee"      ,      "Texas"            ,   
               "Utah"                , "Vermont"       ,       "Virginia"         ,   
               "Washington"          , "West.Virginia",        "Wisconsin"        ,   
                "Wyoming"             , "Puerto.Rico" )

names(wideboy)
good_wide_boy=wideboy%>%
  pivot_longer(cols =-variable, names_to = 'States')%>%
  pivot_wider(names_from = variable, values_from = value)

good_wide_boy%>%
  ggplot(aes(x=income,
             y=rent))+
  geom_point()

table2
df2=table2
view(df2)

df2%>%
  pivot_wider(names_from = type,values_from = count)


df3=table3
df3%>%
  separate(rate, into=c('cases','pop'), convert = T) #convert changes it into a intigers
df4a=table4a
df4b=table4b

merge()
join_by()
dfab=full_join(df4a,df4b)
dfab%>%
  pivot_longer(cols=c('1999','2000'), names_to = 'years', values_to = 'income')

########UGLY PLOT##########
shrek2=read.csv('green_ogre_animated.csv')
shrek2%>%
  ggplot(aes(x=x,
             y=y,
             color=shade))+
  geom_point(size = 0.6) +
  scale_color_gradient(c("#8b0000", "#ff0000", "#ff7a00", "#ffee00", "#ffffff")) +
  coord_fixed() +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "#ff4500"))+
  transition_manual(frame)+
  labs(x='ljlzhf',
       y='asjbjha',
       color='ljk;hk',
       title = 'GOblErrrRRRrRrRrrRRrr')+
  theme(axis.title.x = element_text(color = 'aquamarine', angle = 199, family = 'Comic Sans MS', size=25))+
  theme(axis.title.y = element_text(color = 'chartreuse', angle = 37, family = 'Comic Sans MS', size=20))+
  theme(title = element_text(color = '#CFFF04', angle = 180, base.family = 'Comic Sans MS', size=30, vjust = 1))+
  theme(panel.grid.major.y = element_line(color = '#2A52BE') )+
  theme(panel.grid.major.x = element_line(color = '#FF6EC7') )

anim_save('Assignments/Assignment_5/Ugly_Plot_BULLOCK.gif')



#10/22/26####
df4a=table4a
view(df4a)
df4b=table4b
view(df4b)

df4ag=df4a%>%
  pivot_longer(cols = -country, names_to = 'year')%>%
  rename('cases'= 'value')
  
df4bg=df4b%>%
  pivot_longer(cols = -country, names_to = 'year')%>%
  rename('population'= 'value')

full_join(df4ag,df4bg)

df5=table5
view(df5)
df5%>%
  separate(rate, into = c('case','population'),convert = T)%>%
  mutate(year=paste0(df5$century,df5$year)%>%
  as.numeric())%>%
  select(-century)
library(readxl) 
library(janitor)
df=read_xlsx(
  sheet=3,#this is how you skip sheets in xl
  skip=17,#this is how you skil rows
  range=E13:F16)#this will select the specific area that you want

#janitor
make_clean_names('# of bacteria')#this will clean specific names
clean_names()#of the whole sheet
read_xlsx('Data/messy_bp.xlsx')


#10/27/26####
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
messy_bp=read_xlsx('Data/messy_bp.xlsx',
                   skip=3)
# combine month day year.

Clean_BP=messy_bp%>%
  mutate(Date_of_Birth=paste(`Month of birth`,`Day birth`,`Year birth`, sep='-'))%>%
  select(-`Month of birth`,-`Day birth`,-`Year birth`)%>%
  select(-starts_with('HR'))%>%
  pivot_longer(cols = starts_with('BP'),
               names_to = 'visit',
               values_to = 'BP')%>%
  mutate(visit = case_when(visit== 'BP...8'~'1',
                           visit== 'BP...10'~'2',
                           visit== 'BP...12'~'3'))%>%
 separate(BP, into = c('Systolic', 'Diastolic'), convert = T)




Clean_HR=messy_bp%>%
  mutate(Date_of_Birth=paste(`Month of birth`,`Day birth`,`Year birth`, sep='-'))%>%
  select(-`Month of birth`,-`Day birth`,-`Year birth`)%>%
  select(-starts_with('BP'))%>%
  pivot_longer(cols=starts_with('HR'),
               names_to= 'visit',
               values_to = 'HR')%>%
  mutate(visit = case_when(visit== 'HR...9'~'1',
                           visit== 'HR...11'~'2',
                           visit== 'HR...13'~'3'))
full_Clean_BP=full_join(Clean_BP,Clean_HR)

full_Clean_BP%>%
  ggplot(aes(x=visit,
             y= HR,
             color= Race,
             group=pat_id))+
  geom_path()+
  facet_wrap(~Race)


#10/29/26####
library(skimr)
BM=read.csv('Data/Bird_Measurements.csv')
view(BM)
BM_keep=skim(BM)
KEEP=BM_keep%>%
  filter(complete_rate>=.67)%>%
  pluck('skim_variable')
cleanish=BM%>%
  select(KEEP, F_tarsus)

F_cleanish=cleanish%>%
  select(-starts_with('M_'))%>%
  mutate(Sex='Female')%>%
  rename(Body_Mass=F_mass, Wing=F_wing, Tarsus=F_tarsus, Bill=F_bill)
  
  
M_cleanish=cleanish%>%
  select(-starts_with('F_'),)%>%
  mutate(Sex='Male')%>%
  rename(Body_Mass=M_mass, Wing=M_wing, Tarsus=M_tarsus, Bill=M_bill)

full_cleanish=full_join(M_cleanish, F_cleanish)
full_cleanish%>%
  ggplot(aes(x=Egg_mass,
             y=Body_Mass,
             color=Sex))+
  geom_point()+
  geom_smooth(method="lm", formula = y~poly(x,2))+
  scale_x_continuous(limits = c(0,600))


penguins%>%
  ggplot(aes(x=body_mass_g,
             fill=species))+
  geom_density(alpha=0.5)
  
#11/05/26#####

penguins%>%
  drop_na()%>%
  ggplot(aes(x=body_mass_g,
             fill=sex))+
  geom_density(alpha=0.5)
t.test(penguins$body_mass_g~penguins$sex)
summary(aov(formula = body_mass_g~species, data=penguins))#this will tell you what you want aobut three or more means but it doens't tell you how much
mod=glm(formula = body_mass_g~species, data=penguins)#this will give you better results than anova tests

penguins%>%
  drop_na(body_mass_g)%>%
  group_by(species)%>%
  summarize(mean=mean(body_mass_g))


relevel()#helps reorder the data
penguins$species=relevel(penguins$species,ref='Gentoo')#this now starts with gentoo instead of adelie
report(glm(formula = body_mass_g~species, data=penguins))
performance(glm(formula = body_mass_g~species, data=penguins))
check_model(glm(formula = body_mass_g~species, data=penguins))
mod$model$body_mass_g
mod$effects
mod$coefficients
mod$fitted.values

mpg
car=mpg
view(car
     )
mod2=glm(formula=cty~displ, data=car)
summary(mod2)
report(mod2)
check_model(mod2)

car%>%
  ggplot(aes(x=displ,
             y=cty))+
  geom_smooth(method='lm')+
  geom_point()+
  annotate(geom='text',
           x=6,y=30, label='R^2=0.64')

#practice for stuff I missed####

glm(data = mpg,
    formula = x + y + z + x:y + x*y)


mpg

mod1 = glm(data = mpg,
           formula = displ + cyl)

mod_h = glm(data = mpg,
            formula = cty ~ displ + drv + cyl)

mod_hm = glm(data = mpg,
             formula = cty ~ displ + trans)

summary(mod_hm)

mod1 = glm(data = mpg, formula = cty ~ displ)
mod2 = glm(data = mpg, formula = cty ~ displ + cyl)
mod3 = glm(data = mpg, formula = cty ~ displ*cyl)

summary(mod3)


mpg %>% 
  ggplot(aes(x = displ, y = cty)) +
  geom_point() +
  geom_smooth(method = 'glm')


mpg %>% 
  ggplot(aes(x = displ, y = cty, color = factor(cyl))) +
  geom_point() +
  geom_smooth(method = 'glm')


mpg %>% 
  ggplot(aes(x = displ, y = cty, color = cyl)) +
  geom_point() +
  geom_smooth(method = 'glm')

#11/06/26####

install.packages('devtools')
library(devtools)
devtools::install_github("nstrayer/datadrivencv")

