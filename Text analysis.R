#Clinical trials text analysis
#set working directory
setwd('C:/Users/Admin/Documents/PROJECTS/TEXT ANALYSIS/Clinical trials Tex/Clinicals_Text')


library(dplyr)
library(ggplot2)
library(tidytext)
library(wordcloud)
library("XML")
library(lubridate)

#Load Clinical Trials in Kenya Data Set
clinicals <- xmlToDataFrame("KenyaTrials.xml")

#Transform Date Column from character to Date variable
class(clinicals$Date_registration) ##'character'
#using the lubridate library
clinicals <- clinicals %>%
  mutate(Date_registration = lubridate::dmy(Date_registration))

class(clinicals$Date_registration)##'Date'

names(clinicals)

#What to do?
## Clinical profile of conditions on trial
## analysis on titles
## ethical review committees 
## profile on source support


