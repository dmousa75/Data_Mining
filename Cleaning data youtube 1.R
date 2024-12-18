
install.packages("tidyverse")
library(tidyverse)

data()

class(starwars$gender)

starwars$gender<- as.factor(starwars$gender)

starwars$gender <- factor((starwars$gender),
                          levels = c("masculine", "feminine"))
starwars$gender

#select variables

starwars %>% 
  select(name,height, ends_with ("color")) %>% 
  names() #print nama variables/column sahaja

#Filter observations

starwars %>% 
  filter(hair_color %in% c("blond", "brown") 
         & height < 180  )
  names()

#missing data

starwars$height
mean(starwars$height, na.rm =TRUE)


starwars %>% 
  select(name,gender, hair_color,height) %>% 
  filter(complete.cases(.))

starwars %>% # check for 'na' that in all columns and rows
  select(name,gender, hair_color,height) %>% 
  filter(!complete.cases(.))

#if we want to remove 'na' from specific colums

starwars %>% # check for 'na' that in all columns and rows
  select(name,gender, hair_color,height) %>% 
  drop_na(height) %>% 
  View()

#if we want to replace with something values (mutate= write over or change an existing variable)
starwars %>% # check for 'na' that in all columns and rows ( %>% = that's in)
  select(name,gender, hair_color,height) %>% 
  filter(!complete.cases(.)) %>% 
  mutate(hair_color = replace_na(hair_color, "none"))
  


#duplicates


starwars %>% 
  distinct() %>% 
  View()

starwars$name %>% distinct() 
starwars$height %>% distinct() 

#starwars1$name %>% distinct()


#recoding values in variables

starwars %>% 
  select(name,gender) %>% 
  mutate(gender = recode(gender, "masculine" = 1, "feminine" = 2))
