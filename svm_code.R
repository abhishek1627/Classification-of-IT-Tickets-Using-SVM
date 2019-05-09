library(NLP)
library(tm)
library(SnowballC)
#library(slam)
#library(stringi)
library("dplyr")
library(stringr)
library(e1071)
#install.packages('caret')
library(caret)
#install.packages("pROC")
library(pROC)
library(reshape2)
#install.packages("plyr")
#library(plyr)

#Read the file
txtdata <- read.csv("sd2.csv")

View(txtdata)
HB <- subset(txtdata,select=Summary)
vertical<-as.data.frame(txtdata$vertical)
head(vertical)
HB.corpus <- Corpus(DataframeSource(HB))
docs <- HB.corpus


docs
#* desc<-as.character(txtdata$Summary)
#* vertical<-as.data.frame(txtdata$vertical)
#cl_desc1<-as.data.frame(cl_desc1)
#class(vertical)
#cl_desc = gsub("[[:punct:]]", "", desc)
#cl_desc = gsub("[[:digit:]]", "", cl_desc)
#cl_desc<- as.data.frame(cl_desc)
#cl_desc$ver <- ver
#str(cl_desc)

# Load the data as a corpus


#* docs <- Corpus(VectorSource(desc))


# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Text stemming
docs1 <- tm_map(docs, stemDocument, language = "english")  # Stemming
# Remove numbers
# docs1 <- tm_map(docs, removeNumbers)   


## DTM TF-IDF
DTM1 <- DocumentTermMatrix(docs1, control = list(weighting = function(x) weightTfIdf(x, normalize =FALSE),stopwords = FALSE))
DTM2 <- DocumentTermMatrix(docs1)
# DTM1$dimnames 
# DTM11 <- as.data.frame(inspect(DTM1))

## TDM TF-TDF
TDM1 <- TermDocumentMatrix(docs1, control = list(weighting = function(x) weightTfIdf(x, normalize =FALSE),stopwords = FALSE))
TDM2 <- TermDocumentMatrix(docs1)

# removing sparse variables
DTM_SP <- removeSparseTerms(DTM1, 0.9999) # 0.9365; 0.9523 (without vars) ***

# DTM_SP <- removeSparseTerms(DTM1, 0.99992101) # 0.9124 ; 0.9106

# inspect(DTM1) # selects only 10*10 values
DTM_SP_55<-as.data.frame(as.matrix(DTM_SP))
## add target variable
View(DTM_SP_55)
names(vertical)<- "vertical"
DTM_SP_55$ver1<- vertical$vertical

#creating training test set
# DTM55_Univ <- DTM_SP_55
# index<- sample(1:nrow(DTM55_Univ),0.7*nrow(DTM55_Univ))
# DTM55_Train<-DTM55_Univ[index,]
# DTM55_Test<-DTM55_Univ[-index,]

# two test sets
# t_ind<- sample(1:nrow(DTM55_Test),0.5*nrow(DTM55_Test))
# DTM55_t_1<-DTM55_Test[t_ind,]
# DTM55_t_2<-DTM55_Test[-t_ind,]
# 

#SVM Modeling: BASE MODEL

# Linear
#Svm_DTM5 <- svm (formula= ver1~. ,data=DTM55_Train, kernel="linear")
#Svm_DTM5 <- svm (formula= ver1~. ,data=DTM55_Train, kernel="linear", cost=0.1, gamma=0.000503)
#Svm_DTM5 <- svm (formula= ver1~. ,data=DTM55_Train, kernel="linear", cost=0.05, gamma=0.000503)

#Svm_DTM3 <- svm (formula= ver1~. ,data=DTM55_Train, kernel="linear", cost=0.02, gamma=0.000503)

# Radial
#Svm_DTM5 <- svm (formula= ver1~. ,data=DTM55_Train, cost=1, gamma=0.000503) 
#Svm_DTM5 <- svm (formula= ver1~. ,data=DTM55_Train, cost=10, gamma=0.000503) 
#Svm_DTM5 <- svm (formula= ver1~. ,data=DTM55_Train, cost=20, gamma=0.000503) 
#Svm_DTM5 <- svm (formula= ver1~. ,data=DTM55_Train, cost=30, gamma=0.000503)
#Svm_DTM3 <- svm (formula= ver1~. ,data=DTM55_Train, cost=30, gamma=0.000503)

#Prediction of test dataset 1
#predictions1 <- as.numeric(predict(Svm_DTM3, DTM55_t_1, type = 'response'))
#multiclass.roc(DTM55_t_1$ver1, predictions1)

#Prediction of test dataset 2
#predictions2 <- as.numeric(predict(Svm_DTM3, DTM55_t_2, type = 'response'))
#multiclass.roc(DTM55_t_2$ver1, predictions2)


############################FEATURE ENGINEERING####################################

## add target variable
##Insignificant variables3k
# dtm_v1$Priority2 <- txtdata$Priority2  #Variable not significant
#dtm56$Status <- txtdata$Status  #Variable not significant
#dtm_v1$IMPACTED.APPLICATION <- txtdata$IMPACTED.APPLICATION 
#dtm_v1$IMPACTED.SERVICE <- txtdata$IMPACTED.SERVICE 
# dtm_v1$REGION <- txtdata$REGION 

##Significant
#dtm_v1$Opened_By_Workgroup <- as.factor(tolower(txtdata$Opened_By_Workgroup))
# dtm_v1$Escalation.Group <- as.factor(tolower(txtdata$Escalation.Group))
# dtm_v1$Escalation.Sub.Group <- as.factor(tolower(txtdata$Escalation.Sub.Group))

# dtm_v1$COUNTRY_NAME <- as.factor(tolower(txtdata$COUNTRY_NAME))
# dtm_v1$Month <- as.factor(tolower(txtdata$Month_Name_Long))

#creating training test set
dtm_v1 <- DTM_SP_55
DTM55_Univ <- dtm_v1
set.seed(99)
index<- sample(1:nrow(DTM55_Univ),0.75*nrow(DTM55_Univ)) # with 0,70: 0.938; 0.9456  # with 0.82: 0.9598; 0.9515  *
DTM55_Train<-DTM55_Univ[index,]
DTM55_Test<-DTM55_Univ[-index,]

# index <- sample(1:nrow(Data2_x1),1.0*nrow(Data2_x1),)


# Increasing CRM in Train (although CRM is dominant class in target variable)
# Data1 <- DTM55_Train[DTM55_Train$ver1 == "CRM",]
# set.seed(99)
# index <- sample(1:nrow(Data1),0.90*nrow(Data1))
# Data1<-Data1[index,]
# DTM55_Train <- rbind(DTM55_Train, Data1)  #

# two test sets
set.seed(11)
t_ind<- sample(1:nrow(DTM55_Test),0.5*nrow(DTM55_Test))
DTM55_t_1<-DTM55_Test[t_ind,] 
DTM55_t_2<-DTM55_Test[-t_ind,]
# 
# #Test datasets
# 
# trainIndex <- createDataPartition(, p=split, list=FALSE)
# data_train <- iris[ trainIndex,]
# data_test <- iris[-trainIndex,]

##SVM Modeling with best Cost, Gamma ad significant variables added
set.seed(20)
Svm_DTM7 <- svm (formula= ver1~. ,data=DTM55_Train, cost=33, gamma=0.000503)

predictions1 <- as.numeric(predict(Svm_DTM7, DTM55_t_1, type = 'response'))
multiclass.roc(DTM55_t_1$ver1, predictions1)
predictions2 <- as.numeric(predict(Svm_DTM7, DTM55_t_2, type = 'response'))
multiclass.roc(DTM55_t_2$ver1, predictions2)

# 0.9423, 0.9431 (seeds: 99,11; cost=30, gamma=0.000503; sample 0.75)
# 0.943, 0.943 (seeds: 99,11, 20 for SVM; cost=33, gamma=0.000503; sample 0.75)
# Increasing CRM in Train worked previously (although CRM is dominant class in target variable)



## Confusion matrix
pred <- predict(Svm_DTM7,DTM55_t_1)
y <- DTM55_t_1$ver1
tab<- table(pred,y)
confusionMatrix(tab)
pred <- predict(Svm_DTM7,DTM55_t_2)
y <- DTM55_t_2$ver1
tab<- table(pred,y)
confusionMatrix(tab)



######################  END ###############################


##########################################
## Adding 2 Grams using n-gram tokenizer
## but it did not work
##########################################

library("RWeka")

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

DTM_NG2 <- DocumentTermMatrix(docs1, control = list(tokenize = BigramTokenizer,
                                                    weighting = function(x) weightTfIdf(x, normalize =FALSE),stopwords = FALSE))


# removing sparse variables
DTM_SP_NG2 <- removeSparseTerms(DTM_NG2, 0.999) # 0.9365; 0.9523 (without vars) ***

# DTM_SP <- removeSparseTerms(DTM1, 0.99992101) # 0.9124 ; 0.9106

# inspect(DTM1) # selects only 10*10 values
DTM_SP_55_NG2<-as.data.frame(as.matrix(DTM_SP_NG2))



## merging 2 grams with previous data
DTM_SP_55_NG12 <- cbind(DTM_SP_55, DTM_SP_55_NG2)


############################FEATURE ENGINEERING####################################


dtm_v1 <- DTM_SP_55_NG12


#creating training test set
DTM55_Univ <- dtm_v1
set.seed(99)
index<- sample(1:nrow(DTM55_Univ),0.75*nrow(DTM55_Univ)) # with 0,70: 0.938; 0.9456  # with 0.82: 0.9598; 0.9515  *
DTM55_Train<-DTM55_Univ[index,]
DTM55_Test<-DTM55_Univ[-index,]


# two test sets
set.seed(11)
t_ind<- sample(1:nrow(DTM55_Test),0.5*nrow(DTM55_Test))
DTM55_t_1<-DTM55_Test[t_ind,] 
DTM55_t_2<-DTM55_Test[-t_ind,]


##SVM Modeling with best Cost, Gamma ad significant variables added
set.seed(20)
Svm_DTM7 <- svm (formula= ver1~. ,data=DTM55_Train, cost=33, gamma=0.000503)

predictions1 <- as.numeric(predict(Svm_DTM7, DTM55_t_1, type = 'response'))
multiclass.roc(DTM55_t_1$ver1, predictions1)

predictions2 <- as.numeric(predict(Svm_DTM7, DTM55_t_2, type = 'response'))
multiclass.roc(DTM55_t_2$ver1, predictions2)
