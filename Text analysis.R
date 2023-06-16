#Clinical trials text analysis
#set working directory
setwd('C:/Users/Admin/Documents/PROJECTS/TEXT ANALYSIS/Clinical trials Tex/Clinicals_Text')


library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(wordcloud)
library("XML")
library(lubridate)
library(naniar)
library(janitor)

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


# -- 14th/06/2023
# First task is look at the nature of Conditions that are within the clinical trials.
# We'll look at the last 5 years

# create a data frame for the last 5 years.
five_years <- clinicals[clinicals$Date_registration >= "2017-06-14" & clinicals$Date_registration <= "2023-06-01", ]
fiveyears_conditions <- five_years[ , c('TrialID','Date_registration','Condition')]

# Look for missing values
pct_miss(fiveyears_conditions$Condition) ## 3.921569% of the data set don't have conditions

## Data Cleaning
# drop na
fiveyears_conditions$Condition [fiveyears_conditions$Condition ==""] <- NA
fiveyears_conditions <- fiveyears_conditions %>%
  drop_na(Condition)

# Remove <br>
fiveyears_conditions$Condition <- gsub("<br>","", fiveyears_conditions$Condition)

# WORKING WITH QUANTEDA PACKAGE
#Quanteda
library(quanteda) 
# Tokenise The conditions column
fiveyears_conditions.tokens <- tokens(fiveyears_conditions$Condition, what='word',
                                      remove_numbers = TRUE, remove_punct = TRUE,
                                      remove_symbols = TRUE, remove_separators = TRUE)

fiveyears_conditions.tokens[[200]]

#transform to lower
fiveyears_conditions.tokens <- tokens_tolower(fiveyears_conditions.tokens)

# remove stopwords 
fiveyears_conditions.tokens <- tokens_select(fiveyears_conditions.tokens, stopwords(),selection = 'remove')
fiveyears_conditions.tokens[[200]]

#stemwords
fiveyears_conditions.tokens <- tokens_wordstem(fiveyears_conditions.tokens, language = 'english')
fiveyears_conditions.tokens[[200]]

#create a bag of words model
fiveyears_conditions.tokens.dfm <- dfm(fiveyears_conditions.tokens, tolower=FALSE)

#TRANSFORM TO MATRIX
fiveyears_conditions.tokens.matrix <- as.matrix(fiveyears_conditions.tokens.dfm)
View(fiveyears_conditions.tokens.matrix[1:20, 1:100])
dim(fiveyears_conditions.tokens.matrix)


fiveyears_conditions.tokens %>% 
  count(word, sort=TRUE)



# WORKING WITH TIDYTEXT
#Tidy Text
tidy_conditions <-  fiveyears_conditions%>%
  unnest_tokens(word, Condition)

#Using stop words
data(stop_words)
tidy_conditions <- tidy_conditions %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))

#visualizing words
tidy_consitions %>%
  count(word, sort=TRUE) %>%
  filter(n > 50)%>%
  mutate(word=reorder(word, n))%>%
  ggplot(aes(n, word))+
  geom_col()+
  labs(y=NULL)




















