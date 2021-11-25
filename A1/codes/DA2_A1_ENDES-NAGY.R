rm(list=ls())

#packages
library(tidyverse)
library(fixest)
library(modelsummary)
library(estimatr)
library(lspline)

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
df <- df %>% 
  filter( occ == 1 | occ == 0 ) %>% 
  filter( grade92 >= 43 )

#generate variables for female and wage
df <- df %>% mutate( female = as.numeric( sex == 2 ) ) %>%
  mutate( w = earnwke/uhours ) %>%
  mutate( lnw = log( w ) ) %>% 
  mutate( BA_degree = ifelse( grade92 == 43 , 1 , 0 ) ) %>% 
  mutate( MA_degree = ifelse( grade92 == 44 , 1 , 0 ) ) %>% 
  mutate( Prof_degree = ifelse( grade92 == 45 , 1 , 0 ) ) %>% 
  mutate( PhD_degree = ifelse( grade92 == 46 , 1 , 0 ) )

## DESCRIPTIVE STATISTICS
P95 <- function(x){quantile(x,0.95,na.rm=T)}
P05 <- function(x){quantile(x,0.05,na.rm=T)}
Range <- function(x){max(x,na.rm=T)-min(x,na.rm=T)}

datasummary( w + lnw + age ~ 
               Mean + SD + Median + Min + Max + Range + P05 + P95 + N , 
             data = df)

#let's have a look on conditional distribution of wage by sex
ggplot( data = df , aes( x = female, y = w )) +
  geom_violin(aes (group = female ) , trim = F) + 
  geom_boxplot(aes (group = female ) , width = 0.1) +
  scale_x_continuous(label = c('male','female') , breaks = c(0,1)) +
  scale_y_continuous( limits = c(0,150)) +
  labs( x = '' , 
        y = 'Hourly wage (USD)' ,
        title = 'Hourly wages in legal jobs, by sex' ,
        caption = 'data from: https://osf.io/4ay9x') +
  stat_summary(fun = mean, geom = 'point') +
  theme(
    panel.background = element_rect( fill = 'white' ) ,
    panel.grid.major = element_line( size = 0.1, linetype = 'solid',
                                     colour = 'azure3') ,
    panel.grid.minor = element_line( size = 0.1, linetype = 'dotted',
                                     colour = 'azure4') ,
    plot.subtitle = element_text( size = 10 ) )


#check relationship between age and lnw see if linear
ggplot( data = df , aes( x = age, y = lnw )) +
  geom_point(  size = .75, shape = 16, ) + 
  geom_smooth( method = 'loess', formula = y ~ x , size = .5) +
  labs( x = 'Age' , 
        y = 'ln(Hourly wage (USD))' ,
        title = 'Hourly log wages in legal jobs, by age' ,
        caption = 'data from: https://osf.io/4ay9x') +
  scale_x_continuous(breaks = seq(15,65,by=5)) +
  theme(
    panel.background = element_rect( fill = 'white' ) ,
    panel.grid.major = element_line( size = 0.1, linetype = 'solid',
                                     colour = 'azure3') ,
    panel.grid.minor = element_line( size = 0.1, linetype = 'dotted',
                                     colour = 'azure4') ,
    plot.subtitle = element_text( size = 10 ) )


#REGRESSION MODELS
#simple
reg1 <- lm(lnw ~ female, data = df)
summary(reg1)

#with occupation type (cool VS supporting jobs)
reg2 <- lm(lnw ~ occ + female , data = df)
summary(reg2)

# add level of education. Bachelor's degree is the reference
reg3 <- lm(lnw ~ occ + female +  MA_degree + Prof_degree + PhD_degree , data = df)
summary(reg3)


#add age with lspline, 35 looked like a decent point on the above graph
reg4 <- lm(lnw ~ occ + female + MA_degree + Prof_degree + PhD_degree + lspline( age , 35), data = df)
summary(reg4)


msummary( list( reg1 , reg2 , reg3 , reg4 ) ,
         fmt="%.4f",
         gof_omit = 'DF|Deviance|Log.Lik.|F|R2 Adj.|AIC|BIC',
         stars=c('*' = .05, '**' = .01)
)

df$lnw_hat1 <- predict(reg1)
df$lnw_hat2 <- predict(reg2)
df$lnw_hat3 <- predict(reg3)
df$lnw_hat4 <- predict(reg4)

df$lnw_res1 <- df$lnw - df$lnw_hat1
df$lnw_res2 <- df$lnw - df$lnw_hat2
df$lnw_res3 <- df$lnw - df$lnw_hat3
df$lnw_res4 <- df$lnw - df$lnw_hat4


ggplot(df, aes( x = lnw , y = lnw_hat4) ) + 
  geom_point( size = 0.1 ) +
  geom_point( aes( y = lnw_hat1 , color = as.factor(female) ) ) +
  geom_segment(aes(x = 2.5, y = 2.5, xend = 4 , yend = 4 ) , color = 'navyblue') +
  ylim( 2.5 , 4) +
  labs( x = 'ln(hourly wage, USD)' , y = 'ln(predicted hourly wage, USD)')
 

ggplot(df, aes( x = lnw_hat4 , y = lnw_res4 ) ) + 
  geom_point( size = 0.2 ) +
  geom_smooth( method="lm", colour='blue', se=F , formula = y~x , size = .5) +
  labs( caption = 'data from: https://osf.io/4ay9x' ,
        x = 'ln(Predicted hourly wage)' , 
        y = 'Residual', 
        title = 'Fitness of Model 4' , 
        subtitle = 'Residuals; ln(hourly wage) prediction based on sex, occupation, education and age ') +
  theme(
    panel.background = element_rect( fill = 'white' ) ,
    panel.grid.major = element_line( size = 0.1, linetype = 'solid',
                                     colour = 'azure3') ,
    panel.grid.minor = element_line( size = 0.1, linetype = 'dotted',
                                     colour = 'azure4') ,
    plot.subtitle = element_text( size = 10 )
  )


