library(tidyverse)
library(fixest)
library(modelsummary)
library(estimatr)

#import full dataset
df <- read_csv('https://osf.io/4ay9x/download', 
               col_types = cols(.default = "?", 
                                state = "c"))

##CLEANING and FILTERING
#differentiate between higher level (1) and supporting jobs (0) in the legal field
df <- df %>% 
  mutate( occ = ifelse( occ2012 == 2100 , 1 , 
                           ifelse( df$occ2012 >= 2105 & df$occ2012 <= 2160 , 0 , 999 ) ) )

#filter out other occupations (999) and level of education below Bachelor's degree
df <- df %>% filter( occ == 1 | occ == 0 ) %>% 
  filter( grade92 >= 43 )

#generate variables for female and wage
df <- df %>% mutate( female = as.numeric( sex == 2 ) ) %>%
  mutate( w = earnwke/uhours ) %>%
  mutate( lnw = log( w ) ) %>% 
  mutate( BA_degree = ifelse( grade92 == 43 , 1 , 0 ) ) %>% 
  mutate( MA_degree = ifelse( grade92 == 44 , 1 , 0 ) ) %>% 
  mutate( Prof_degree = ifelse( grade92 == 45 , 1 , 0 ) ) %>% 
  mutate( PhD_degree = ifelse( grade92 == 46 , 1 , 0 ) ) %>%
  mutate( agesq = age ^ 2 ) 

## DESCRIPTIVE STATISTICS
P95 <- function(x){quantile(x,0.95,na.rm=T)}
P05 <- function(x){quantile(x,0.05,na.rm=T)}
Range <- function(x){max(x,na.rm=T)-min(x,na.rm=T)}




#REGRESSION MODELS
#simple
reg1 <- lm_robust(lnw ~ female, data = df, se_type = "HC1")
summary(reg1)

#with occupation type (cool VS supporting jobs)
reg2 <- lm_robust(lnw ~ occ + female , data = df, se_type = "HC1")
summary(reg2)

# add level of education. Bachelor's degree is the reference
reg3 <- lm_robust(lnw ~ occ + female +  MA_degree + Prof_degree + PhD_degree , data = df, se_type = "HC1")
summary(reg3)

#add age
reg4 <- lm_robust(lnw ~ occ + female +  MA_degree + Prof_degree + PhD_degree + age , data = df, se_type = "HC1")
summary(reg4)

#add age square to see if non-linear
reg5 <- lm_robust(lnw ~ occ + female + MA_degree + Prof_degree + PhD_degree + age + agesq, data = df, se_type = "HC1")
summary(reg5)



msummary( list( reg1 , reg2 , reg3 , reg4 , reg5 ) ,
         fmt="%.4f",
         gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC|Std.Errors',
         stars=c('*' = .05, '**' = .01)
)
