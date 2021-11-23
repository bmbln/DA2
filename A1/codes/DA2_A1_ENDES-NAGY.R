library(tidyverse)
library(fixest)

#import full dataset
df <- read_csv('https://osf.io/4ay9x/download', 
               col_types = cols(.default = "?", 
                                state = "c"))

##CLEANING and FILTERING
#differentiate between higher level (1) and supporting jobs (0) in the legal field
df <- df %>% 
  mutate( occ = ifelse( occ2012 == 2100 , 1 , 
                           ifelse( df$occ2012 >= 2105 & df$occ2012 <= 2160 , 0 , 999 ) ) )

#filter out other occupations (999)
df <- df %>% filter( occ == 1 | occ == 0 )

#generate variables for female and wage
df <- df %>% mutate( female = as.numeric( sex == 2 ) ) %>%
  mutate( w = earnwke/uhours ) %>%
  mutate( lnw = log( w ) ) %>%
  mutate( agesq = age ^ 2 )



