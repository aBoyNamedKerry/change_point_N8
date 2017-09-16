# Script for simple changeponit analysis and simulating data

library(tidyverse)
library(changepoint)

#simulate some data data fro AMOC

#Create the number of weeks
set.seed(6)

week<- c(1:24)

#simulate offences for the first week using a poisson distribution
violent_off1<- rpois(n = 12, lambda = 6)

#then second week
violent_off2<- rpois(n = 12, lambda = 3)

#we can combine these together really easily using the 'c'
violent_off<- c(violent_off1, violent_off2)

#and create a data.frame object
crime_df<- data.frame(week = week, violent_off = violent_off)

#lets see what it looks like in the console
head(crime_df)

#let's see what the data looks like
ggplot(data = crime_df)+
  geom_line(aes(x = week, y = violent_off, group = 1), colour = "red")+
  labs(y = "count of offences", title = "Violent offences by week")+
  theme_bw()


#lets plot where the intervention took place
ggplot(data = crime_df)+
      geom_line(aes(x = week, y = violent_off, group = 1), colour = "red")+
      labs(y = "count of offences", title = "Violent offences by week")+
      geom_vline(xintercept = 12, colour = "blue")+
      annotate(geom = "text", x =  15, y = 10, label = "Police Intervention")+
      theme_bw()

#deploy changepoint analysis with default settins
mean_changepoint<- changepoint::cpt.mean(crime_df$violent_off)

#print fucntion summary
summary(mean_changeponit)

#plot where the changepoint occurred.
plot(mean_changepoint, ylab = "Number of offences", 
     main = "Change Point in violent offences following police intervention")

#simple plot for show
ggplot(data = crime_df)+
  geom_line(aes(x = week, y = violent_off, group = 1), colour = "red")+
  labs(y = "count of offences", title = "Violent offences by week")+
  theme_classic()
