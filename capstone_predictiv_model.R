
#The goal of the second week assignment of the  Coursera Data Scienence Capstone  project  is to build a simple model for the 
#relationship between words,as a predictive text mining application. We will explore simple  n-gram models,
#for predicting the next word based on the previous one and two words



# Initalization

 start_time<-Sys.time()

 lbs<-c("tm", "class","plyr","wordcloud","RWeka","stringr","caret")
 lapply(lbs, require,character.only=TRUE)
 
 pathname<-"C:/Users/kkolaro/Desktop/Private/ucenje/Coursera/Capstone/swift/files"
 setwd(pathname)
 
 # Files summary statistics
 
 files<-list.files(pattern = "en_US")  # List of filies to be analyzed 
 
 
 linestoread <- 20000

 
 read_line<-function(x) {
   s<-readLines(x,warn=F,n=linestoread,encoding = "UTF-8")
   
 }

 samplefctr<-.5

 
 enblogs <- read_line(files[1])
 enblogs <- sample(enblogs, size=length(enblogs)*samplefctr, replace=FALSE)
 
 ennews <- read_line(files[2])
 ennews <- sample(ennews, size=length(ennews)*samplefctr, replace=FALSE)
 
 entwitter <- read_line(files[3])
 entwitter <- sample(entwitter, size=length(entwitter)*samplefctr, replace=FALSE)
 
 
 
 
 en_all <- c(enblogs,ennews,entwitter)
 en_all<-sample(en_all, size=length(en_all), replace=FALSE)
 
 TestPart <- createDataPartition(seq_len(NROW(en_all)), p=0.2, list=FALSE)
 test<-en_all[TestPart] # test data set
 train<-en_all[-TestPart] # training data set
 

 train <- gsub("[^[:alnum:][:blank:]]", "", train)
 train<-gsub(" *\\b[[:alpha:]]{1}\\b *", " ", train) # removing single letters
 train <- gsub("ø", "", train)
 
 test <- gsub("[^[:alnum:][:blank:]]", "", test)
 test<-gsub(" *\\b[[:alpha:]]{1}\\b *", " ", test) # removing single letters
 test <- gsub("ø", "", test)
 

 
# Cleaning text and Corpus creation 
 
 corpus_train<-Corpus(VectorSource(train)) #Creating Corpus - collection of documents 
 corpus_test<-Corpus(VectorSource(test)) #Creating Corpus - collection of documents 
 
 

 
## Text(corpus) cleaning 
 
corpuscleaning<-function(corp){
   
   corp<-tm_map(corp,content_transformer(tolower))
   corp<-tm_map(corp,removeNumbers)
   corp<-tm_map(corp,stripWhitespace)
   corp<-tm_map(corp,removeWords,stopwords("english"))
   return(corp)
}


corpus_train<-corpuscleaning(corpus_train) # Corpus- after the "cleaning"
corpus_test<-corpuscleaning(corpus_test) # Corpus- after the "cleaning"



corpus_train<-sapply(corpus_train,as.character)# Corpus to character
train_df <- data.frame(corpus_train,stringsAsFactors = FALSE)

corpus_test<-sapply(corpus_test,as.character)# Corpus to character
test_df <- data.frame(corpus_test,stringsAsFactors = FALSE)




ngramf<-function(df,n) {
  ngram<-data.frame(table(NGramTokenizer(df,Weka_control(min=n, max=n))))
  return(ngram)
}





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

test_1<-ngramf(test_df,1)
test_2<-ngramf(test_df,2)
test_3<-ngramf(test_df,3)

testFq_3<-table(test_3[, "Freq"])
testFq_2<-table(test_2[, "Freq"])

d3t<-SimpleGT(testFq_3)
d2t<-SimpleGT(testFq_2)

test_3 <- test_3[order(-test_3$Freq),]
test_2 <- test_2[order(-test_2$Freq),]



train_3$word1<-word(train_3$Var1,1)
train_3$word2<-word(train_3$Var1,2)
train_3$word3<-word(train_3$Var1,3)

train_2$word1<-word(train_2$Var1,1)
train_2$word2<-word(train_2$Var1,2)

test_3$word1<-word(test_3$Var1,1)
test_3$word2<-word(test_3$Var1,2)
test_3$word3<-word(test_3$Var1,3)

test_2$word1<-word(test_2$Var1,1)
test_2$word2<-word(test_2$Var1,2)

# model accuaracy, comparing 3 gram model train to test data sets
n=1
while(n<=nrow(test_3)){
  
  test_3$predict[n]<-train_3$word3[train_3$word1==test_3$word1[n] & train_3$word2==test_3$word2[n]][1]
  
  n<-n+1  
}
test_3$correct<-ifelse(test_3$word3==test_3$predict,1,0)
sum(test_3$correct, na.rm=TRUE)/nrow(test_3)*100

#Word predicting function

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


