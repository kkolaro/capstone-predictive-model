#The goal of the second week assignment of the  Coursera Data Scienence Capstone  project  is to build a simple model for the 
#relationship between words,as a predictive text mining application. We will explore simple  n-gram models,
#for predicting the next word based on the previous one and two words.



# Initalization
start_time <- Sys.time()


library(RWeka)
library(tm)
library(class)
library(plyr)
library(stringr)
library(caret)


#pathname<-"C:/Users/kkolaro/Desktop/Private/ucenje/Coursera/Data products/week4project/Capstone/capstone"#
#setwd(pathname)



d3<-readRDS(file="d3.rda")
d2<- readRDS(file="d2.rda")


train_1<-readRDS(file="train_1.rda")
train_2<- readRDS(file="train_2.rda")
train_3<- readRDS(file="train_3.rda")


# Word prediction function

wordprediction<-function(x) {
  
    wordlist<-unlist(str_split(x," "))
    n<-length(wordlist)
    towwords<-c(wordlist[n-1],wordlist[n])
    same<-train_3[which(train_3$word1==towwords[1]& train_3$word2==towwords[2]),]
  
if(nrow(same)>0) {
      
      sap<-merge(same,d3,by.x="Freq", by.y="r")[-(6:10)]
      sap<-sap[order(-sap$p),]
      thirdword<-as.vector(sap[,"word3"])
      
      return(thirdword[1])
    

                  }
      
  
      
       singleword<-wordlist[n]
       same2<-train_2[which(train_2$word1==singleword),]
       
      if(nrow(same2)>0) {
         
            sap<-merge(same2,d2,by.x="Freq", by.y="r")[-(5:8)]
            sap<-sap[order(-sap$p),]
            secondword<-as.vector(sap[,"word2"])
            return(secondword[1])
         
          }
  
        wordfreq<-train_1[order(-train_1$Freq),]
        singlew<-as.vector(wordfreq[,"Var1"])
        # I could potentially return the most frequent single word, but probability to match is extremely low, so  will return NULL
        if(x=="") {
          return(NULL)}
        else { 
        return("Don't know, sorry")
        }
      }
          
end_time <- Sys.time()
print(end_time-start_time)


ready <- function( ){
  if(end_time>0) {
  return("START NOW!")
}
return("")
}
  
  
 



  
 



