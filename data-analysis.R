library("tidyverse")
str(surveys)
select(surveys,species_id,weight)
filter(surveys,year==1995)
#Pipes:   %>% 

surveys %>% 
  filter(year==1995) %>% 
  select(species_id,weight)

#select only the rows with species weight less than 5, then select only the speciesId
#,sex and weight and save it in a new dataset
exercise <- surveys %>% 
  filter(weight<5) %>% 
  select(species_id,sex,weight) 

#create a new column
#mutate

data_with_kgs <- surveys %>% 
  mutate(weight_kg=weight/1000)

surveys %>% 
  mutate(weight_kg=weight/1000,weight_kg2=weight_kg*2) %>% 
  head()

surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg=weight/1000,weight_kg2=weight_kg*2) %>% 
  head()


surveys %>% 
  group_by(sex) %>% 
  summarise(mean_weight=mean(weight,na.rm=TRUE))


#Group by two comlumns
surveys %>% 
  filter(!sex=="") %>% 
  group_by(sex) %>% 
  summarise(mean_weight=mean(weight,na.rm=TRUE))

surveys %>% 
  filter(sex=="M" | sex=="F") %>% 
  group_by(sex) %>% 
  summarise(mean_weight=mean(weight,na.rm=TRUE))

surveys %>% 
  filter(!sex=="") %>% 
  filter(!is.na(weight)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight=mean(weight,na.rm=TRUE))

#Use the previous commands to add new columns that give the minimum and maximum weight
summarise_minweight <- surveys %>% 
  filter(!sex=="") %>% 
  filter(!is.na(weight)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight=mean(weight,na.rm=TRUE),min_weight=min(weight),max_weight=max(weight))
  

#counting the categorical vairables like the species... eg. finding the frequency etc

surveys %>% 
   filter(!sex=="") %>% 
    count(sex)

surveys %>% 
  filter(!sex=="") %>% 
  count(sex,species)

#sorting : arrange()

surveys %>% 
  filter(!sex=="") %>% 
  count(sex,species) %>% 
  arrange(species,desc(n))


#Reshaping the data
