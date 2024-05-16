# Clinical trials text analysis
# set working directory
# setwd('C:/Users/Admin/Documents/PROJECTS/TEXT ANALYSIS/Clinical trials Tex/Clinicals_Text')


library(tidyr) # data management
library(ggplot2) # data visualization
library(wordcloud) # word clouds
library("XML") # read xml files
library(lubridate) # manipulate dates
library(naniar) # missing values
library(janitor) # data cleaning and tables
library(stringr) # work with strings
library(quanteda) # qualitative data analysis
library(ggthemes) #themes
library(extrafont)# import system fonts
extrafont::loadfonts(device = "win")


#Load Clinical Trials in Kenya Data Set
clinicals <- xmlToDataFrame("KenyaTrials.xml")

#Transform Date Column from character to Date variable
class(clinicals$Date_registration) ##'character'
#using the lubridate library
clinicals <- clinicals %>%
  mutate(Date_registration = lubridate::dmy(Date_registration))

class(clinicals$Date_registration)##'Date'

# change column names to lower case
names(clinicals)
clinicals <- janitor::clean_names(clinicals)
names(clinicals)

#What to do?
# General Objectives
## Look at conditions registered 
## analysis on titles
## ethical review committees 
## profile on source support


# -- Data was uploaded on 14th/06/2023
# First task is look at scope of conditions that are within the Clinical Trials.
# We'll look at data over the last 5 years


# Create a data frame for Clinical Trials conducted over the last 5 years
# Obtain the following columns: Trial ID, Date of Registration, Scientific Title and condition

five_years <- clinicals[clinicals$date_registration >= "2019-01-01" & clinicals$date_registration <= "2023-06-01", ]
years_a <- format(as.Date(five_years$date_registration, format="%Y/%m/%d"),"%Y")
years_a <- as.numeric(years_a)
five_years <- cbind(five_years, years_a)

# data frame Containing Clinical Trials from 2018.
fiveyears_conditions <- five_years[ , c('trial_id','years_a','condition','scientific_title')]



## Data Cleaning
# Look for missing values
pct_miss(fiveyears_conditions$condition) ## 3.767123% of the data set don't have conditions

# drop na
fiveyears_conditions$condition [fiveyears_conditions$condition ==""] <- NA
fiveyears_conditions <- fiveyears_conditions %>%
  drop_na(condition)

# Remove symbols, duplicates... etc from conditions column
fiveyears_conditions <- unique( fiveyears_conditions[ , c('trial_id','years_a','condition','scientific_title') ] )

fiveyears_conditions$condition <- tolower(fiveyears_conditions$condition)

fiveyears_conditions$condition <- gsub("<br>","", fiveyears_conditions$condition)
fiveyears_conditions$condition <- gsub(";\\s",";", fiveyears_conditions$condition)
fiveyears_conditions$condition <- gsub("[.,;:-]"," ", fiveyears_conditions$condition)
fiveyears_conditions$condition <- gsub("\\(|\\)", "", fiveyears_conditions$condition)
fiveyears_conditions$condition <- gsub("coronavirus|coronavirus 2|sars cov 2|covid\\s+19","covid", fiveyears_conditions$condition)
fiveyears_conditions$condition <- gsub("health condition 1"," ", fiveyears_conditions$condition)
fiveyears_conditions$condition <- gsub("hiv/aids|hiv 1|aids","hiv", fiveyears_conditions$condition)
fiveyears_conditions$condition <- gsub("sickle cell disease|scd","sickle", fiveyears_conditions$condition)
fiveyears_conditions$condition <- gsub("infections","infection", fiveyears_conditions$condition)
fiveyears_conditions$condition <- gsub("diseases","disease", fiveyears_conditions$condition)
fiveyears_conditions$condition <- gsub("disorders","disorder", fiveyears_conditions$condition)
fiveyears_conditions$condition <- gsub("[0-9]+"," ", fiveyears_conditions$condition)
fiveyears_conditions$condition <- gsub("\\b[a-zA-Z]\\b", " ", fiveyears_conditions$condition)
# Function to remove duplicate words in a string
remove_duplicates <- function(string) {
  string <- trimws(string)
  words <- strsplit(string, " ")[[1]]
  unique_words <- unique(words)
  result <- paste(unique_words, collapse = " ")
  return(result)
}
# Apply the function to the 'conditions' column
fiveyears_conditions$condition <- sapply(fiveyears_conditions$condition, remove_duplicates)


## Text Mining
# Create a corpus of the document

corp_conditions <- corpus(fiveyears_conditions, text_field = 'condition') #initialize corpus for conditions
                          
summary(corp_conditions,3)
#create a new id for the variables
docid <- paste(corp_conditions$trial_id,
               corp_conditions$years_a,
               corp_conditions$scientific_title,
               sep= " ")
docnames(corp_conditions) <- docid
head(docvars(corp_conditions))


# create tokens
corp <- tokens(corp_conditions, what='word',
               remove_punct = TRUE, remove_numbers = TRUE,
               remove_symbols = TRUE, remove_separators = TRUE)
# stem
# corp <- tokens_wordstem(corp, language = 'en')

# remove stopwords 
corp <- tokens_select(corp, stopwords(),selection = 'remove')
corp[[24]]
#construct a Document Feature Matrix
corp_dfm <- dfm(corp)


#corp_features <- top features(corp_dfm, 10)
#corp_features$feature <- with(corp_features, reorder(feature, -frequency))


# Creating Word Clouds
library("quanteda.textplots")
col <- sapply(seq(0.1, 1, 0.1), function(x) adjustcolor("#0B2545", x))
par(bg = "#98BAE3") # set background color
textplot_wordcloud(corp_dfm, rotation = 0, color = col)
## remember to alter font

# How the distribution of Condition happened across the 5 years
# 5 years data
word_freq <- textstat_frequency(corp_dfm, groups = fiveyears_conditions$years_a)

# Plot the frequency text plot
library(quanteda.textstats)
# frequency plot of the top 10 most frequent word
features_corpdfm <- textstat_frequency(corp_dfm, n=10)
features_corpdfm$feature <- with(features_corpdfm, reorder(feature, -frequency))

ggplot(features_corpdfm, aes(feature, frequency))+
  geom_point(size=5, color="#0B1354")+
  labs (title = "Top 10 Word Frequency Plot",
        subtitle = "Conditions 2018-2023",
        y = "",
        x = "")+
  theme_few()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=17, face="bold"))



# frequency plot of most occurring words each year
top_words <- word_freq %>%
  group_by (group) %>%
  top_n(5, frequency)  # Change the number to show more or fewer words

# Create scatter plot
par(bg = "#BACAD5")
ggplot(data = top_words, aes(x = feature, y = frequency)) +
  geom_point(size = 3, fill="#BACAD5") +
  labs(x = "", y = "", title = "Conditions Spread Over 5 years") +
  facet_wrap(~ group, ncol = 3, scales = 'free')+
  theme_few()+
  theme(plot.background = element_rect(fill = "#BACAD5"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=15, face="bold"))# Creates a grid of plots, 2 columns






fiveyears_conditions[c(7,8),]
























# EXPLORATION

# WORKING WITH QUANTEDA PACKAGE
#Quanteda

##We can create a corpus or a bag of word model(dtm)
## let's start with corpus
#create a corpus
five_corpus <- corpus(fiveyears_conditions, text_field = 'condition')
five_corpus

five_corpus.stats <- summary(five_corpus)
head(five_corpus.stats, n=10)
#create tokens
corpus_tokens <- tokens(five_corpus,
                        remove_punct = TRUE,
                        remove_symbols = TRUE, remove_separators = TRUE)
corpus_tokens[[24]]



## Let's create a Token from Fiveyears_condition then DTM
# Tokenize The conditions column
five_tokens <- tokens(fiveyears_conditions$condition, what='word',
                                      remove_punct = TRUE,
                                      remove_symbols = TRUE, remove_separators = TRUE)
five_tokens[[24]]

#Let's go through a data transformation pipeline
# remove stopwords 
five_tokens <- tokens_select(five_tokens, stopwords(),selection = 'remove')
five_tokens[[24]]



# Create A dfm
five_tokensdfm <- dfm(five_tokens)

#TRANSFORM TO MATRIX
five_tokensdfmatrix<- as.matrix(five_tokensdfm)
View(five_tokensdfmatrix[1:20, 1:100])install.packages("quanteda.textmodels")
install.packages("quanteda.textstats")
install.packages("quanteda.textplots")
dim(five_tokensdfmatrix)




corp_conditions <- corpus(fiveyears_conditions, text_field = 'condition')
summary(corp_conditions,5)

docid <- paste(corp_conditions$TrialID,
               corp_conditions$Date_registration, sep= " ")

docnames(corp_conditions) <- docid
print(corp_conditions)

docvars(corp_conditions)

corp <- tokens(corp_conditions, what='word',
               remove_punct = TRUE,
               remove_symbols = TRUE, remove_separators = TRUE)
# remove stopwords 
corp <- tokens_select(corp, stopwords(),selection = 'remove')
corp[[24]]
#keywords in tokens
key_corp <- kwic(corp, pattern='hiv','aids')
head(key_corp, 10)
#creating own dictionary
dict <- dictionary(list(cds = c("hiv*", "aids*","infect*","disease*"),
                        ncds = c("sarco*", "cancer*")))
print(dict)

dict_corp <- tokens_lookup(corp, dictionary = dict)
print(dict_corp)

corp_ngram <- tokens_ngrams(corp, n = 2:4)
head(corp_ngram[[1]], 30)
#construct a dfm
corp_dfm <- dfm(corp)

topfeatures(corp_dfm, 10)

#feature co-ocurrance matrix
fcmat_corp <- fcm(corp)
topfeatures(fcmat_corp)

#select features of an fcmat
feat <- names(topfeatures(fcmat_corp, 50))
fcmat_corp_select <- fcm_select(fcmat_corp, pattern = feat, selection = "keep")
dim(fcmat_corp_select)


size <- log(colSums(dfm_select(corp_dfm, feat, selection = "keep")))

set.seed(144)
library("quanteda.textplots")
textplot_network(fcmat_corp_select, min_freq = 0.8, vertex_size = size / max(size) * 3)

textplot_wordcloud(corp_dfm)














































# WORKING WITH TIDYTEXT
#Tidy Text
tidy_conditions <-  fiveyears_conditions%>%
  unnest_tokens(word, condition)

#Using stop words
data(stop_words)
tidy_conditions <- tidy_conditions %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))

#visualizing words
five_corpus %>%
  count(Tokens, sort=TRUE) %>%
  filter(n > 50)%>%
  #mutate(word=reorder(word, n))%>%
  ggplot(aes(n, word))+
  geom_col()+
  labs(y=NULL)





















