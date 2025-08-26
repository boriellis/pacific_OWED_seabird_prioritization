#this is the new holding area that will become the script that does the whole workflow analysis using the functions in the R folder. right now it's just notes on things I'll need to do to make all those play right together. 


#to load in the iucn values, use this:
library(janitor)

iucn <- read_csv("data/raw_data/raw_iucn_list.csv") %>% clean_names()

#need to load in the sp list and the iucn list before using join_statuses function