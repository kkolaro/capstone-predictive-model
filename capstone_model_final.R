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

train_df<- readRDS(file="train_df.rda")

# n-grams

ngramf<-function(df,n) {
  ngram<-data.frame(table(NGramTokenizer(df,Weka_control(min=n, max=n))))
  return(ngram)
}

# Smoothing

SimpleGT <- function(table_N){
  #Simple Good Turing Algorithm - Gale And Simpson
  #Good Turing Smoothing
  
  # table_U is a table of frequency of frequencies
  # The frequencies are stored as names of the list in the table structure
  # the values are the frequency of frequencies.
  # In Good Turing Smoothing, we are concerned with the frequency of frequencies
  # So, to extract the number of times that words of frequency n occur in the training set, we need to do:
  # table(freq_B)[[as.character(pairCount)]]
  # In a tables with a number of holes or non-contiguous sequence of frequency of words,
  # we can compute N(c+1) as the mean of all frequencies that occur more than Nc times
  # to do this, create a vector that is in the numerical form of the names of the table
  
  # create a data table
  # r is the frequencies of various trigrams
  #n is the frequency of frquencies
  SGT_DT <- data.frame(r=as.numeric(names(table_N)),n=as.vector(table_N),Z=vector("numeric",length(table_N)), 
                       logr=vector("numeric",length(table_N)),
                       logZ=vector("numeric",length(table_N)),
                       r_star=vector("numeric",length(table_N)),
                       p=vector("numeric",length(table_N)))
  #p=vector("numeric",length(table_N)),key="r")
  
  str(SGT_DT)
  
  num_r <- nrow(SGT_DT)
  for (j in 1:num_r) {
    if(j==1) {r_i<-0} else {r_i <- SGT_DT$r[j-1]}
    if(j==num_r){r_k<-SGT_DT$r[j]} else {r_k <- SGT_DT$r[j+1]}
    SGT_DT$Z[j] <- 2*SGT_DT$n[j] / (r_k-r_i)
    #print(paste(r_i,j,r_k))
  }
  SGT_DT$logr <- log(SGT_DT$r)
  SGT_DT$logZ <- log(SGT_DT$Z)
  linearFit <- lm(SGT_DT$logZ ~ SGT_DT$logr)
  print(linearFit$coefficients)
  c0 <- linearFit$coefficients[1]
  c1 <- linearFit$coefficients[2]
  
  plot(SGT_DT$logr, SGT_DT$logZ)
  abline(linearFit,col="red")
  
  use_y = FALSE
  for (j in 1:(num_r-1)) {
    r_plus_1 <- SGT_DT$r[j] + 1
    
    s_r_plus_1 <- exp(c0 + (c1 * SGT_DT$logr[j+1]))
    s_r <- exp(c0 + (c1 * SGT_DT$logr[j]))
    y<-r_plus_1 * s_r_plus_1/s_r
    
    if(use_y) {
      SGT_DT$r_star[j] <- y
    } else { 
      n_r_plus_1 <- SGT_DT$n[SGT_DT$r == r_plus_1]
      n_r <- SGT_DT$n[j]
      x<-(r_plus_1) * n_r_plus_1/n_r
      
      if (abs(x-y) > 1.96 * sqrt(((r_plus_1)^2) * (n_r_plus_1/((n_r)^2))*(1+(n_r_plus_1/n_r)))) {
        SGT_DT$r_star[j] <- x
      }else {
        SGT_DT$r_star[j] <- y
        use_y = TRUE
      }
    }
    if(j==(num_r-1)) {
      SGT_DT$r_star[j+1] <- y
    }
    
  }
  N <- sum(SGT_DT$n * SGT_DT$r)
  Nhat <- sum(SGT_DT$n * SGT_DT$r_star)
  Po <- SGT_DT$n[1] / N
  SGT_DT$p <- (1-Po) * SGT_DT$r_star/Nhat
  
  return(SGT_DT)
  
}



#N-grams train data

train_1<-ngramf(train_df,1)
train_2<-ngramf(train_df,2)
train_3<-ngramf(train_df,3)

trainFq_3<-table(train_3[, "Freq"])
trainFq_2<-table(train_2[, "Freq"])

d3<-SimpleGT(trainFq_3)
d2<-SimpleGT(trainFq_2)

train_3 <- train_3[order(-train_3$Freq),]
train_2 <- train_2[order(-train_2$Freq),]

train_3$word1<-word(train_3$Var1,1)
train_3$word2<-word(train_3$Var1,2)
train_3$word3<-word(train_3$Var1,3)

train_2$word1<-word(train_2$Var1,1)
train_2$word2<-word(train_2$Var1,2)

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
  return("SART NOW!")
}
return("")
}
  
  
 



  
 



