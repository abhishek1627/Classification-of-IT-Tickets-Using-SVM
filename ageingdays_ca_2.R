library(dplyr)
library(devtools)
#install.packages('rsconnect')
library(rsconnect)
# install_github("ropensci/elife")
#library(elife)
library("LDAvis")
library(text2vec)
library(stringr)
getwd()
setwd("C:\\Users\\Abhishek.raj\\Desktop\\text analysis project\\service now")
#library(devtools)
library(sentiment)
library("topicmodels")
library("dplyr")  # Data wrangling, pipe operator %>%().
library("ggplot2")  # Plot word frequencies.
library("wordcloud") #Create a word cloud
library("RColorBrewer")  # Generate palette of colours for plots.
library("tm")  #Framework for Text Mining
library("NLP") #used for processing the language
library("SnowballC") # Helps in collapsing words to a common Root
library("sentimentr") #used to check the sentiments
#install.packages("RGtk2")
#library(RGtk2)
#I didn't even need to restart R-Studio
#Reading both the files into R
ageing_CA<-read.csv("Can_ageingdays.csv")
ageing_GSA<-read.csv("Gsa_ageingdays.csv")


ageing_ca_2nd<-ageing_CA%>%filter(Ageing.days=="4-10")
text_ageing_ca_2<-ageing_ca_2nd$Summary   #to extract the Task description column
text_ageing_ca_2<-gsub("[[:punct:]]", "", text_ageing_ca_2) #to remove the punctuation marks
text_ageing_ca_2 = gsub("[[:digit:]]", "", text_ageing_ca_2) #to remove the digits from the column
text_ageing_ca_2= gsub("https\\w+","",text_ageing_ca_2) #to remove the HTML Links
text_ageing_ca_2= gsub("[ \t]{2,}","",text_ageing_ca_2) # remove unnecessary spaces
text_ageing_ca_2 = gsub("^\\s+|\\s+$","",text_ageing_ca_2) # remove unnecessary spaces

emo.docs_ca_ageing_2<-text_ageing_ca_2
emo.docs_ca_ageing_2 = removeWords(emo.docs_ca_ageing_2, stopwords("english")) #removing stopwords.
myCorpus_CA_ageing_2 <- Corpus(VectorSource(emo.docs_ca_ageing_2))
# myCorpus_CA_ageing_2<- tm_map(myCorpus_CA_ageing_2, stemDocument)
myCorpus_CA_ageing_2<- tm_map(myCorpus_CA_ageing_2, replaceSynonyms, synonyms)
myCorpus_CA_ageing_2$content
tdm_CA_ageing_2<-TermDocumentMatrix(myCorpus_CA_ageing_2)
dtm_CA_ageing_2<-DocumentTermMatrix(myCorpus_CA_ageing_2)
ca_ageing2_mat<-as.matrix(tdm_CA_ageing_2) #converting dtm_ca into a matrix
tdm_CA_ageing_2 = removeSparseTerms(tdm_CA_ageing_2, 0.99)
rowTotals_ca_ageing2 <- apply(tdm_CA_ageing_2 , 1, sum) #Find the sum of words in each Document
tdm.ca.ageing2   <- tdm_CA_ageing_2[rowTotals_ca_ageing2> 0, ]           #remove all docs without words

##################################################################################
dtmrowTotals_ca_ageing2 <- apply(dtm_CA_ageing_2 , 1, sum) #Find the sum of words in each Document
##################################################################################
dtm.ca.ageing2<- dtm_CA_ageing_2[dtmrowTotals_ca_ageing2> 0, ]           #remove all docs without words

#################################################################################

##################################################################################
v_ca_ageing2 <- sort(rowSums(ca_ageing2_mat),decreasing=TRUE)

head(v_ca_ageing2)
##################################################################################
d_ca_ageing2 <- data.frame(word = names(v_ca_ageing2),freq=v_ca_ageing2)

##################################################################################
aa_ca_ageing2<-head(d_ca_ageing2, 1000)

##################################################################################
# wordcloud(words = aa_ca_ageing2$word, freq = aa_ca_ageing2$freq, random.order=FALSE, rot.per=0.35,  colors=brewer.pal(1000,"Dark2"))  #creating a wordcloud for CANADA
##################################################################################
lda_ca_ageing2 <- LDA(dtm.ca.ageing2,k=4,method="Gibbs")

#get topics
lda.topics.ca_ageing2<- as.data.frame(topics(lda_ca_ageing2))
#write.csv(ap_lda2.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))
#topics(lda_ca_ageing2)
#get top 80 terms in each topic
lda_ca_ageing2.terms <- as.data.frame((terms(lda_ca_ageing2,100)))
#write.csv(ap_lda2.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))
##################################################################################
#get probability of each topic in each doc
topicProbabilities_ca_ageing2 <- as.data.frame(lda_ca_ageing2@gamma)

#write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))

##############################Find the ideal number of Topics###############
#Using references
#https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html

library("ldatuning")
library("topicmodels")
# # data("AssociatedPress", package="topicmodels")
# #dtm <- AssociatedPress[1:10, ]
# result <- FindTopicsNumber(
#   dtm.ca.ageing2,
#   topics = seq(from = 2, to = 20, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 100),
#   mc.cores = 2L,
#   verbose = TRUE
# )
# result
# 
# normalize <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }
# 
# 
# 
# result
# FindTopicsNumber_plot(result)
# result$CaoJuan2009<-normalize(result$CaoJuan2009)
# result$Arun2010<-normalize(result$Arun2010)
# result$Griffiths2004<-normalize(result$Griffiths2004)
# result$Deveaud2014<-normalize(result$Deveaud2014)
# Sum_cao_arun<-result$CaoJuan2009+result$Arun2010
# Sum_Gr_dev<-result$Griffiths2004+result$Deveaud2014
# diff<-(Sum_Gr_dev-Sum_cao_arun)
# diff<-as.matrix(diff)
# i<-max(diff, na.rm = TRUE)
# n<-which(diff==i)
# 
# FindTopicsNumber_plot(result)


n=6

#########################TOPIC MODELLING USING SHINY############################

txt_CA_shiny<- as.character(ageing_ca_2nd$Summary)
#This process cannot be applied on Corpus, hence no conversion Required##########

#Data Cleaning and Preprocessing
txt_CA_shiny<-str_replace_all(string = txt_CA_shiny,pattern = "[^[:alnum:]]", replacement = " ")
txt_CA_shiny<- str_replace_all(txt_CA_shiny,"\\s+", " ")

#Remove stopwords
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b') 
txt_CA_shiny<- str_replace_all(txt_CA_shiny,stopwords_regex, "")

#Remove Numbers 
txt_CA_shiny<-gsub('[[:digit:]]+', '',txt_CA_shiny)

# #Remove words with length less than two 
txt_CA_shiny<-gsub('\\b\\w{1,2}\\b','',txt_CA_shiny)

#Remove double spaces 
txt_CA_shiny<- str_replace_all(txt_CA_shiny,"\\s+", " ")

#Creating dataframe for ids
num_shiny_CA<-data.frame(c(1:4704))#11921

#Tokenizing and creating DTM and pruning
tokens_CA_shiny = txt_CA_shiny %>% tolower %>% word_tokenizer
it_shiny_CA = itoken(tokens_CA_shiny, ids =num_shiny_CA , progressbar = FALSE)
v = create_vocabulary(it_shiny_CA) %>% prune_vocabulary(term_count_min = 10, doc_proportion_max = 0.2)
vectorizer = vocab_vectorizer(v)
dtm_shiny_CA = create_dtm(it_shiny_CA, vectorizer, type = "lda_c")
set.seed(1)
#Creating lda model for CANADA
lda_model_shiny_CA = LDA$new(n_topics = n, vocabulary = v, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr_CA = lda_model_shiny_CA$fit_transform(dtm_shiny_CA, n_iter = 1000, convergence_tol = 0.001, check_convergence_every_n = 10)


#Plotting model
lda_model_shiny_CA$plot()
