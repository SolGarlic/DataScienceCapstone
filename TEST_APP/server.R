#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

## Other necessary functions:
library(data.table)
library(tm)
library(SnowballC)

load("C1-4_FULL.Rdata")
CalcProbTermsALT <- function(txt) {
      
      # create indexed and clean 3gram with txt words
      library(tm)
      # clean the text with the same procedure as the training corpus
      # and return the vector with the ngram
      
      txt<-removePunctuation(txt)
      txt<-removeNumbers(txt)
      txt<-tolower(txt)
      txt<-removeWords(txt, tm::stopwords("en"))
      txt<-stemDocument(txt)
      ntxt<- unlist(strsplit(txt," +"))
      
      # source <- VectorSource(txt)
      # PC2 <- VCorpus(source,
      #                readerControl = list(reader = readPlain,
      #                                     language = "en", load = TRUE))
      # PC2 <- cleanCorpus(PC2)
      # ntxt<- unlist(strsplit(PC2[[1]][[1]][1]," +"))
      
      #      ntxt<- unlist(strsplit(txt," "))
      
      n <- length(ntxt)
      ntxt<- tail(ntxt,3) # assumes n>=3
      txt<-match(ntxt,vocab)
      # if 3gram exists in DB, find most common words
      # find group of 4grams with first 3grams == txt
      
      group4<-C4[.(txt[1],txt[2],txt[3])]
      if(sum(is.na(group4$freq)==0)) {
            words4<-head(group4[,.(X=X4, prob=cGT/sum(freq))][order(-prob)],10)
            
            beta<-C3[.(txt[1],txt[2],txt[3])]$leftover
            
            #next, even if 3gram is found, calc prob of words after only 2gram
            group3<-C3[.(txt[2],txt[3])][!(X3 %in% words4$X)]
            alpha<-beta/(sum(group3$cGT)/sum(group3$freq))
            words3<-head(group3[,.(X=X3, prob=alpha*cGT/sum(freq))][order(-prob)],10)
            
            #next, even if 2gram is found, calc prob of words after only 1gram
            group2<-C2[.(txt[3])][!(X2 %in% c(words4$X,words3$X))]
            alpha<-beta/(sum(group2$cGT)/sum(group2$freq))
            words2<-head(group2[,.(X=X2, prob=alpha*cGT/sum(freq))][order(-prob)],10)
            
            #next, also calc prob of single words (really not needed, but)
            group1<-C1[!(X1 %in% c(words4$X,words3$X,words2$X))]
            alpha<-beta/(sum(group1$cGT)/sum(group1$freq))
            words1<-head(group1[,.(X=X1, prob=alpha*cGT/sum(freq))][order(-prob)],10)
            
            words<- rbind(words4, words3, words2, words1)
            words<-words[,.(X=vocab[X],prob)][order(-prob)]
            words$X<-stemCompletion(words$X, StemDictionary$token, type="first")
            return(words)
      }
      
      group3<-C3[.(txt[2],txt[3])]
      if(sum(is.na(group3$freq)==0)) {
            words3<-head(group3[,.(X=X3, prob=cGT/sum(freq))][order(-prob)],10)
            
            beta<-C2[.(txt[2],txt[3])]$leftover
            
            #next, even if 2gram is found, calc prob of words after only 1gram
            group2<-C2[.(txt[3])][!(X2 %in% c(words3$X))]
            alpha<-beta/(sum(group2$cGT)/sum(group2$freq))
            words2<-head(group2[,.(X=X2, prob=alpha*cGT/sum(freq))][order(-prob)],10)
            
            #next, also calc prob of single words (really not needed, but)
            group1<-C1[!(X1 %in% c(words3$X,words2$X))]
            alpha<-beta/(sum(group1$cGT)/sum(group1$freq))
            words1<-head(group1[,.(X=X1, prob=alpha*cGT/sum(freq))][order(-prob)],10)
            
            words<- rbind(words3, words2, words1)
            words<-words[,.(X=vocab[X],prob)][order(-prob)]
            words$X<-stemCompletion(words$X, StemDictionary$token, type="first")
            return(words)
      }
      
      group2<-C2[.(txt[3])][order(-cGT)]
      if(sum(is.na(group2$freq)==0)) {
            words2<-head(group2[,.(X=X2, prob=cGT/sum(freq))][order(-prob)],10)
            
            beta<-C1[.(txt[3])]$leftover
            
            #next, also calc prob of single words (really not needed, but)
            group1<-C1[!(X1 %in% c(words2$X))]
            alpha<-beta/(sum(group1$cGT)/sum(group1$freq))
            words1<-head(group1[,.(X=X1, prob=alpha*cGT/sum(freq))][order(-prob)],10)
            
            words<- rbind(words2, words1)
            words<-words[,.(X=vocab[X],prob)][order(-prob)]
            words$X<-stemCompletion(words$X, StemDictionary$token, type="first")
            return(words)
      }
      
      group1<-C1
      if(sum(is.na(group1$freq)==0)) {
            words1<-head(group1[,.(X=X1, prob=cGT/sum(freq))][order(-prob)],10)
            
            words<- words1
            words<-words[,.(X=vocab[X],prob)][order(-prob)]
            words$X<-stemCompletion(words$X, StemDictionary$token, type="first")
            return(words)
      }
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
      output$oWords = renderPrint({paste(head(CalcProbTermsALT(input$txtID)$X,3),collapse=" ")})
})
