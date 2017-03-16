library(tm)
library(data.table)

# different tokenizers
# tokenizers:     unlist(tokenize_ngrams(PC[[1]][[1]][1:4], n=2L, n_min=2, simplify=TRUE))
# tau:      
# RWeka:          BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
# quanteda:       toks <- tokens(c(text1 = "the quick brown fox jumped over the lazy dog"))
#                 tokens_ngrams(toks, n = 4, concatenator = " ")
# ngram:          ngram_asweka(text1 , min =2 , max =2)

#alternative to createNgram
createNgramIntIndexALT <- function(corpus=PCclean, n=2L) {
      # idea for optimization: create all 1-4 ngrams at the same time with one dtm
      # then divide the data table in 4 columns with the 4 ngrams
      # use the lines with 1grams to create the vocab
      # split the large data table in the 4 Cn's
      BigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = n, max = n))
      tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer,
                                                       wordLengths=c(1,Inf)))
      if (n==1L) vocab <<-tdm$dimnames$Terms # create vocab character vector
      ColNames <- paste("X",1:n, sep="")
      
      tdm <- data.table(token=tdm$dimnames$Terms, freq=as.integer(slam::row_sums(tdm, na.rm = T)))
      tdm <- tdm[!grepl("\\beosent\\b", token)  # remove rows with "eosent" token
                 ][, (ColNames) := tstrsplit(token, " ", fixed=TRUE) # create columns with slipt tokens
                   ][, token:=NULL       # delete original column
                     ][, c(ColNames) :=lapply(.SD, function(x) match(x,vocab)), .SDcols=ColNames]
      # # and last line to convert columns with single tokens to their corresponding index in vocab
      setkeyv(tdm, ColNames)
      if(n==1) C1 <<- tdm[,.N, by="freq"][order(freq)]
      if(n==2) C2 <<- tdm[,.N, by="freq"][order(freq)]
      if(n==3) C3 <<- tdm[,.N, by="freq"][order(freq)]
      if(n==4) C4 <<- tdm[,.N, by="freq"][order(freq)]
      tdm
}

createVocab <- function(corpus, n=1) {
      # creates a vector with all the tokens in a corpus with freq greater than n
      library(data.table)
      options( java.parameters = "-Xmx4g" ) # to solve heap memory problems in RWeka
      BigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))
      tot=length(corpus)
      tdm<-list()
      for (i in 1:tot)  {
      # for (i in 1:15)  {
                  print(i)
            tdm[[i]] <- TermDocumentMatrix(corpus[i], control = list(tokenize = BigramTokenizer,
                                                             wordLengths=c(1,Inf)))
      }
      tdm<-Reduce(c,tdm)
      tdm <- data.table(token=tdm$dimnames$Terms, freq=as.integer(slam::row_sums(tdm, na.rm = T)))
      tdm <- tdm[freq>n
                 ][!grepl("\\beosent\\b", token)
                   ][order(token)]
      # vocab<-tdm$token
      C1 <<- tdm[,.(X1=1:.N, X2=NA, X3=NA, X4=NA, freq, type=1L)]
      vocab <- tdm$token
      C1
}

createNgramIntIndexALTALT <- function(corpus=PCclean, nmax=4L, nmin=2L) {
      # creates all 1-4 ngrams at the same time with one dtm
      # then divide the data table in 4 columns with the 4 ngrams
      # use the lines with 1grams to create the vocab
      # split the large data table in the 4 Cn's by "type"
      options( java.parameters = "-Xmx4g" ) # to solve heap memory problems in RWeka
      BigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = nmin, max = nmax))
      # tdm <-list()
      #have to use lapply and combine because if it processes more than 1 doc at the same time
      # it runs out of memory
      # tdm <- lapply(1:length(PC), function(i) TermDocumentMatrix(PC[i], control = list(tokenize = BigramTokenizer,
      #                                                  wordLengths=c(1,Inf))))
      tot=length(PC)
      ColNames <- paste("X",1:nmax, sep="")
      
      # 60x1 cycles
      # for (i in 1:tot)  {
      #       print(i)
      #       temp <- TermDocumentMatrix(PC[i], control = list(tokenize = BigramTokenizer,
      #                                                        wordLengths=c(1,Inf)))
      #       save(temp, file=paste("_tdm",i,".Rdata", sep=""))
      #       if(i!=tot) rm(temp)
      # }
      
      # 20x3 cycles
      for (j in c(0,20,40))  {
            temp<-list()
            for (i in 1:20)  {
                  print(i+j)
                  temp[[i]] <- TermDocumentMatrix(PC[i+j], control = list(tokenize = BigramTokenizer,
                                                                          wordLengths=c(1,Inf)))
            }
            temp<-Reduce(c,temp)
            temp <- data.table(token=temp$dimnames$Terms, freq=as.integer(slam::row_sums(temp, na.rm = T)))
            # temp <- data.table(token=temp$dimnames$Terms, freq=as.integer(temp$v))
            temp <- temp[, (ColNames) := tstrsplit(token, " ", fixed=TRUE)    # create columns with slipt tokens
                         ][, type:=Reduce(`+`, lapply(.SD,function(x) !is.na(x))), .SDcols=ColNames  # create column with ngram type (number of not NA columns)
                           ][, token:=NULL   # delete original column
                             ][, c(ColNames) :=lapply(.SD, function(x) match(x,vocab)), .SDcols=ColNames # match token to vocab index
                               ][, check:=Reduce(`+`, lapply(.SD,function(x) !is.na(x))), .SDcols=ColNames  # create column check
                                 ][type==check  #and only keep all rows in which type = check (with no additional unknown words)
                                   ][,c("check", "type"):=NULL]  #eliminate check column
            setkeyv(temp,ColNames)
            save(temp, file=paste("_tdmF",j,".Rdata", sep=""))
            rm(temp)
      }      
      
      
      #20 x 3
      for (j in c(0,20,40))  {
            print(j)
            load(file=paste("_tdmF",j,".Rdata", sep=""))
            if (j==0) tdm<-temp
            if (j!=0) {
                  tdm <-rbind(temp, tdm)
                  tdm <-tdm[,.(freq=sum(freq, na.rm=TRUE)),by=ColNames]
                  # tdm<-merge(temp, tdm, by=ColNames, all=TRUE)
                  # tdm[,freq:=rowSums(.SD,na.rm=TRUE), .SDcols=c("freq.x","freq.y")][,c("freq.x", "freq.y"):=NULL]
            }

      }
      # }
      # 60x1
      # for (j in c(0,20,40))  {
      #       for (i in 1:20)  {
      #             print(i+j)
      #             load(file=paste("_tdm",i+j,".Rdata", sep=""))
      #             # temp <- data.table(token=temp$dimnames$Terms, freq=as.integer(slam::row_sums(temp, na.rm = T)))
      #             temp <- data.table(token=temp$dimnames$Terms, freq=as.integer(temp$v))
      #             temp <- temp[, (ColNames) := tstrsplit(token, " ", fixed=TRUE)    # create columns with slipt tokens
      #                          ][, type:=Reduce(`+`, lapply(.SD,function(x) !is.na(x))), .SDcols=ColNames  # create column with ngram type (number of not NA columns)
      #                            ][, token:=NULL   # delete original column
      #                              ][, c(ColNames) :=lapply(.SD, function(x) match(x,vocab)), .SDcols=ColNames # match token to vocab index
      #                                ][, check:=Reduce(`+`, lapply(.SD,function(x) !is.na(x))), .SDcols=ColNames  # create column check
      #                                  ][type==check  #and only keep all rows in which type = check (with no additional unknown words)
      #                                    ][,c("check", "type"):=NULL]  #eliminate check column
      #             setkeyv(temp,ColNames)
      #             if (i==1) tdm<-temp
      #             if (i!=1) {
      #                   tdm <-rbind(temp, tdm)
      #                   tdm <-tdm[,.(freq=sum(freq, na.rm=TRUE)),by=ColNames]
      #                   # tdm<-merge(temp, tdm, by=ColNames, all=TRUE)
      #                   # tdm[,freq:=rowSums(.SD,na.rm=TRUE), .SDcols=c("freq.x","freq.y")][,c("freq.x", "freq.y"):=NULL]
      #             }
      #             if (i==20) {
      #                   save(tdm, file=paste("_tdm20_F",j,".Rdata", sep=""))
      #                   rm(tdm)
      #             }
      #       }
      # }
      # load(file=paste("_tdm20_F",0,".Rdata", sep=""))
      # temp<-tdm
      # load(file=paste("_tdm20_F",20,".Rdata", sep=""))
      # temp<-rbind(temp, tdm)
      # temp<-temp[freq>1]  # needed to be able to bind, but there is no problem because we only want freq>2
      # load(file=paste("_tdm20_F",40,".Rdata", sep=""))
      # temp<-rbind(temp, tdm)
      # tdm<-temp[,.(freq=sum(freq, na.rm=TRUE)),by=.(X1,X2,X3,X4)]
      # rm(temp)
      # tdm<-tdm[freq>2]
      # tdm<-rbind[tdm1,tdm] # tdm1 is table with 1gram calculated together with vocab
      # tdm[, type:=Reduce(`+`, lapply(.SD,function(x) !is.na(x))), .SDcols=ColNames]  # create column with ngram type (number of not NA columns)
      # 
      # setkeyv(tdm, ColNames)
      # C1<<-tdm[type==1][,type:=NULL]
      # C2<<-tdm[type==2][,type:=NULL]
      # C3<<-tdm[type==3][,type:=NULL]
      # C4<<-tdm[type==4][,type:=NULL]
      tdm
}

c_ast <- function(N=N1, k=5, nWords=0,ngramtype=1) {
      # N<-N2; k=5; unigram=FALSE
      # creates a numeric vector of length k+1 (0:k) with the GoodTuring Smooth count
      #      if (nWords==0) {N0 <- 0 ; c_ast0 <- 0}
      #      if (nWords>0) {N0 <- nWords^ngramtype-nWords ; c_ast0 <- (0+1)*N[1]/N0}
      
      N0 <- nWords^ngramtype-nWords
      N=c(N0,N$N)
      Const<- (k+1)*N[k+1+1]/N[1+1]
      c<-0:k
      c_ast <- ((c+1)*N[c+1+1]/N[c+1]- c*Const )/(1-Const)
      names(c_ast) <- 0:k
      c_ast
}

CreateNnALT<- function() {
      sampleDIR(sample=0.6)
      createPC(Virtual = FALSE)
      # PCclean <- cleanCorpus(PC, StemDic=TRUE)
      cleanCorpus(PC, StemDic=TRUE)

      vocab <- createVocab(PC) # also creates C1 with all 1grams
      N1<-C1[,.N, by="freq"][order(freq)]
      save(C1, N1, file="CNsimples1.Rdata")
      rm(C1, N1)
      C2 <- createNgramIntIndexALTALT(PC,2,2)
      N2<-C2[,.N, by="freq"][order(freq)]
      save(C2, N2, file="CNsimples2.Rdata")
      rm(C2, N2)
      C3 <- createNgramIntIndexALTALT(PC,3,3)
      N3<-C3[,.N, by="freq"][order(freq)]
      save(C3, N3, file="CNsimples3.Rdata")
      rm(C3, N3)
      C4 <- createNgramIntIndexALTALT(PC,4,4)
      N4<-C4[,.N, by="freq"][order(freq)]
      save(C4, N4, file="CNsimples4.Rdata")
      rm(C4, N4)
      
      
      
      # alt      
      # C1 <- createNgramIntIndexALT(PC,n=1L) #also creates "vocab" variable
      # C2 <- createNgramIntIndexALT(PC,n=2L)
      # C3 <- createNgramIntIndexALT(PC,n=3L)
      # C4 <- createNgramIntIndexALT(PC,n=4L)

      # altalt 
      # vocab <- createVocab # also creates tdm1 with all ngrams
      # createNgramIntIndexALTALT(PC, n=4) # creates remaining ngrams and incorporates tdm1.
      # returns C1, C2, C3, C4 and ignores freq <=2

      
      # N1<-C1[,.N, by="freq"][order(freq)]
      # N2<-C2[,.N, by="freq"][order(freq)]
      # N3<-C3[,.N, by="freq"][order(freq)]
      # N4<-C4[,.N, by="freq"][order(freq)]

      N1<-rbind(data.table(freq=1, N=3*N1$N[1]),N1)
      
      k=5
      cGT<-data.table(f=0:k
                      ,N1=c_ast(N=N1,k=k,nWords=sum(N1$N),ngramtype=1)
                      ,N2=c_ast(N=N2,k=k,nWords=sum(N1$N),ngramtype=2)
                      ,N3=c_ast(N=N3,k=k,nWords=sum(N1$N),ngramtype=3)
                      ,N4=c_ast(N=N4,k=k,nWords=sum(N1$N),ngramtype=4))
      rm(N1, N2, N3, N4)

      C4<-merge(C4, cGT[,.(f,cGT=N4)], by.x = "freq", by.y="f", all.x = TRUE) # add cGT column
      C4[is.na(cGT), cGT:=as.numeric(freq)]                                      # add cGT column
      setkey(C4, X1, X2, X3, X4)
      leftoverN3 <- C4[ , .(leftover = 1-sum(cGT) / sum(freq)), by = list(X1, X2, X3)]
      setkey(leftoverN3, X1, X2, X3)
      C4<-C4[C4$freq>1,]

      C3<-merge(C3, cGT[,.(f,cGT=N3)], by.x = "freq", by.y="f", all.x = TRUE) # add cGT column
      C3[is.na(cGT), cGT:=as.numeric(freq)]                                      # add cGT column
      setkey(C3, X1, X2, X3)
      leftoverN2<-C3[ ,.(leftover=1-sum(cGT)/sum(freq)), by=list (X1, X2)]
      setkey(leftoverN2, X1, X2)
      C3<-C3[C3$freq>1,]
      C3<-merge(C3, leftoverN3, all.x = TRUE)
      
      C2<-merge(C2, cGT[,.(f,cGT=N2)], by.x = "freq", by.y="f", all.x = TRUE) # add cGT column
      C2[is.na(cGT), cGT:=as.numeric(freq)]                                      # add cGT column
      setkey(C2, X1, X2)
      leftoverN1<-C2[ ,.(leftover=1-sum(cGT)/sum(freq)), by=list (X1)]
      setkey(leftoverN1, X1)
      C2<-C2[C2$freq>1,]
      C2<-merge(C2, leftoverN2, all.x = TRUE)
      
      C1[, c("X2", "X3", "X4") :=NULL]
      
      C1<-merge(C1, cGT[,.(f,cGT=N1)], by.x = "freq", by.y="f", all.x = TRUE) # add cGT column
      C1[is.na(cGT), cGT:=as.numeric(freq)]                                      # add cGT column
      setkey(C1, X1)
      # C1$type=1L
      C1<-C1[C1$freq>1,]
      C1<-merge(C1, leftoverN1, all.x = TRUE)
      
      rm(leftoverN1, leftoverN2, leftoverN3, temp2, N1, N2, N3, N4)

      # After this we have all we need to calculate probabilites:
      # - vocab with an index of all found words
      # - C1...C4, indexed ngram tables, with count, smoothed count and leftover probability
      # - StemDictionary with an "unstemmed" version of stemmed words, sorted by frequency, for stemcompletion

      c<-1
      save(c, file="xxx_end.Rdata")
      
}

ReverseIndexLevels <- function(df, v=vocab) { 
      df<-as.data.frame(df)
      ncol=dim(df)[2]
      for(i in 1:ncol) {
            df[,i]<-vocab[df[,i]]
      }
      foo<-apply(df, 1, function(x) paste(x, collapse=" "))
      foo
}

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
            words<-stemCompletion(words, StemDictionary$token, type="first")
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
            words<-stemCompletion(words, StemDictionary$token, type="first")
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
            words<-stemCompletion(words, StemDictionary$token, type="first")
            return(words)
      }

      group1<-C1
      if(sum(is.na(group1$freq)==0)) {
            words1<-head(group1[,.(X=X1, prob=cGT/sum(freq))][order(-prob)],10)
            
            words<-words[,.(X=vocab[X],prob)][order(-prob)]
            words<-stemCompletion(words, StemDictionary$token, type="first")
            return(words)
      }
}


CalcProbALT <- function(txt="white house officials",words=c("said", "spoke", "told", "announced")) {
      
      # create indexed and clean 3gram with txt words
      library(tm)
      # clean the text with the same procedure as the training corpus
      # and return the vector with the ngram
      source <- VectorSource(txt)
      PC2 <- VCorpus(source,
                     readerControl = list(reader = readPlain,
                                          language = "en", load = TRUE))
      PC2 <- cleanCorpus(PC2)
      ntxt<- unlist(strsplit(PC2[[1]][[1]][1]," +"))
      #      ntxt<- unlist(strsplit(txt," "))
      n <- length(ntxt)
      ntxt<- tail(ntxt,3) # assumes n>=3
      txt<-match(ntxt,vocab)
      
      #convert the words to indexes
      words<-stemDocument(words)
      words<-match(words,vocab)
      
      # if 3gram exists in DB, find most common words
      # find group of 4grams with first 3grams == txt
      
      group4<-C4[.(txt[1],txt[2],txt[3])]
      if(sum(is.na(group4$freq)==0)) {
            words4<-group4[X4 %in% words,.(X=X4, prob=cGT/sum(freq))][order(-prob)]
            
            beta<-C3[.(txt[1],txt[2],txt[3])]$leftover
            words<-words[!(words %in% words4$X)]
            #next, even if 3gram is found, calc prob of words after only 2gram
            group3<-C3[.(txt[2],txt[3])][!(X3 %in% words4$X)]
            alpha<-beta/(sum(group3$cGT)/sum(group3$freq))
            words3<-group3[X3 %in% words,.(X=X3, prob=alpha*cGT/sum(freq))][order(-prob)]
            words<-words[!(words %in% words3$X)]
            
            #next, even if 2gram is found, calc prob of words after only 1gram
            group2<-C2[.(txt[3])][!(X2 %in% c(words4$X,words3$X))]
            alpha<-beta/(sum(group2$cGT)/sum(group2$freq))
            words2<-group2[X2 %in% words,.(X=X2, prob=alpha*cGT/sum(freq))][order(-prob)]
            words<-words[!(words %in% words2$X)]
            
            #next, also calc prob of single words (really not needed, but)
            group1<-C1[!(X1 %in% c(words4$X,words3$X,words2$X))]
            alpha<-beta/(sum(group1$cGT)/sum(group1$freq))
            words1<-group1[X1 %in% words,.(X=X1, prob=alpha*cGT/sum(freq))][order(-prob)]
            
            words<- rbind(words4, words3, words2, words1)
            words<-words[,.(X=vocab[X],prob)][order(-prob)]
            return(words)
      }
      
      group3<-C3[.(txt[2],txt[3])]
      if(sum(is.na(group3$freq)==0)) {
            words3<-group3[X3 %in% words,.(X=X3, prob=cGT/sum(freq))][order(-prob)]
            words<-words[!(words %in% words3$X)]
            
            beta<-C2[.(txt[2],txt[3])]$leftover
            
            #next, even if 2gram is found, calc prob of words after only 1gram
            group2<-C2[.(txt[3])][!(X2 %in% c(words3$X))]
            alpha<-beta/(sum(group2$cGT)/sum(group2$freq))
            words2<-group2[X2 %in% words,.(X=X2, prob=alpha*cGT/sum(freq))][order(-prob)]
            words<-words[!(words %in% words2$X)]
            
            #next, also calc prob of single words (really not needed, but)
            group1<-C1[!(X1 %in% c(words3$X,words2$X))]
            alpha<-beta/(sum(group1$cGT)/sum(group1$freq))
            words1<-group1[X1 %in% words,.(X=X1, prob=alpha*cGT/sum(freq))][order(-prob)]
            
            words<- rbind(words3, words2, words1)
            words<-words[,.(X=vocab[X],prob)][order(-prob)]
            return(words)
      }
      
      group2<-C2[.(txt[3])]
      if(sum(is.na(group2$freq)==0)) {
            words2<-group2[X2 %in% words,.(X=X2, prob=cGT/sum(freq))][order(-prob)]
            words<-words[!(words %in% words2$X)]
            
            beta<-C1[.(txt[3])]$leftover
            
            #next, also calc prob of single words (really not needed, but)
            group1<-C1[!(X1 %in% c(words2$X))]
            alpha<-beta/(sum(group1$cGT)/sum(group1$freq))
            words1<-group1[X1 %in% words,.(X=X1, prob=alpha*cGT/sum(freq))][order(-prob)]
            
            words<- rbind(words2, words1)
            words<-words[,.(X=vocab[X],prob)][order(-prob)]
            return(words)
      }
      
      group1<-C1
      if(sum(is.na(group1$freq)==0)) {
            words1<-group1[X1 %in% words,.(X=X1, prob=alpha*cGT/sum(freq))][order(-prob)]
            
            words<-words[,.(X=vocab[X],prob)][order(-prob)]
            return(words)
      }
}




CalcProb <- function(txt) {
      # returns the probability of the last word in the txt variable
      library(tm)
      # clean the text with the same procedure as the training corpus
      # and return the vector with the ngram
      # this version uses a CC table in which the first column contains all txt ngrams 
      source <- VectorSource(txt)
      PC2 <- VCorpus(source,
                     readerControl = list(reader = readPlain,
                                          language = "en", load = TRUE))
      PC2 <- cleanCorpus(PC2)
      ntxt<- unlist(strsplit(PC2[[1]][[1]][1]," +"))
      #      ntxt<- unlist(strsplit(txt," "))
      n <- length(ntxt)
      ntxt<- tail(ntxt,n)
      
      #      IndexLevels(ntxt) # replace the text by its word indices if we will use indices instead of text
      
      ngramw <- sapply(1:4, function(x) paste(ntxt[x:4], collapse=" "))
      ngramm1  <- sapply(1:3, function(x) paste(ntxt[x:3], collapse=" "))
      ngram<- c(ngramw, ngramm1)
      pos<-match(ngram, CC$ngramtxt)
      
      # first we check if the w-3,w-1 ngram is found
      if(!is.na(pos[5])) {          # w-3,w-1 ngram found, we can continue
            #calc prob 4
            if(!is.na(pos[1])) {          # exists w-3,w ngram
                  Cnum<- CC$cGT[pos[1]]   # discounted freq of w-3,w ngram
                  Cden<- CC$freq[pos[5]]  # freq of w-3,w-1 ngram
                  return(data.frame(Prob=Cnum/Cden, ngram=ngram[1], type="11",stringsAsFactors = FALSE))}
            
            #calc prob 3
            if(!is.na(pos[2])) {          # exists w-2,w ngram
                  Cnum<- CC$cGT[pos[2]]   # discounted freq of w-2,w ngram
                  Cden<- CC$freq[pos[6]]  # freq of w-2,w-1 ngram
                  beta <- CC$leftover[pos[2]]  #leftover of w-2,w ngram
                  #calculate alpha
                  group2gram <- CC[CC$nm1gram==ngram[6],] # ngrams which contain the first terms
                  group3gram <- CC[CC$nm1gram==ngram[5],] # ngrams which contain the first terms
                  # ngrams with contain the last terms and didn't exist in the above order
                  group<-group2gram[!(gsub(".* ","",group2gram$ngramtxt) %in% 
                                            gsub(".* ","",group3gram$ngramtxt)),]
                  alpha <-beta/sum((group$cGT)/Cden)
                  return(data.frame(Prob=alpha*Cnum/Cden, ngram=ngram[1], type="12",stringsAsFactors = FALSE))}
            
            #calc prob 2
            if(!is.na(pos[3])) {          # exists w-1,w ngram
                  Cnum<- CC$cGT[pos[3]]   # discounted freq of w-1,w ngram
                  Cden<- CC$freq[pos[7]]  # freq of w-1,w-1 ngram
                  beta <- CC$leftover[pos[3]]  #leftover of w-1,w ngram
                  #calculate alpha
                  group2gram <- CC[CC$nm1gram==ngram[7],] # ngrams which contain the first terms
                  group3gram <- CC[CC$nm1gram==ngram[5],] # ngrams which contain the first terms
                  # ngrams with contain the last terms and didn't exist in the above order
                  group<-group2gram[!(gsub(".* ","",group2gram$ngramtxt) %in% 
                                            gsub(".* ","",group3gram$ngramtxt)),]
                  alpha <-beta/sum((group$cGT)/Cden)
                  return(data.frame(Prob=alpha*Cnum/Cden, ngram=ngram[1], type="13",stringsAsFactors = FALSE))}
            
            #calc prob 1
            if(!is.na(pos[4])) {          # exists w ngram
                  Cnum<- CC$cGT[pos[4]]   # discounted freq of w ngram
                  Cden<- sum(CC$freq[CC$type==1])  # sum of all 1grams
                  beta <- CC$leftover[pos[4]]  #leftover of w ngram
                  #calculate alpha
                  group2gram <- CC[CC$type==1,] # ngrams which contain the first terms
                  group3gram <- CC[CC$nm1gram==ngram[5],] # ngrams which contain the first terms
                  # ngrams with contain the last terms and didn't exist in the above order
                  group<-group2gram[!(gsub(".* ","",group2gram$ngramtxt) %in% 
                                            gsub(".* ","",group3gram$ngramtxt)),]
                  alpha <-beta/sum((group$cGT)/Cden)
                  return(data.frame(Prob=alpha*Cnum/Cden, ngram=ngram[1], type="14",stringsAsFactors = FALSE))}
      }
      if(!is.na(pos[6])) {          # w-2,w-1 ngram found, we can continue
            #calc prob 4
            if(!is.na(pos[2])) {          # exists w-2,w ngram
                  Cnum<- CC$cGT[pos[2]]   # discounted freq of w-2,w ngram
                  Cden<- CC$freq[pos[6]]  # freq of w-2,w-1 ngram
                  return(data.frame(Prob=Cnum/Cden, ngram=ngram[1], type="21",stringsAsFactors = FALSE))}
            
            #calc prob 3
            if(!is.na(pos[3])) {          # exists w-1,w ngram
                  Cnum<- CC$cGT[pos[3]]   # discounted freq of w-1,w ngram
                  Cden<- CC$freq[pos[7]]  # freq of w-1 ngram
                  beta <- CC$leftover[pos[3]]  #leftover of w-1,w ngram
                  #calculate alpha
                  group2gram <- CC[CC$nm1gram==ngram[7],] # ngrams which contain the first terms
                  group3gram <- CC[CC$nm1gram==ngram[6],] # ngrams which contain the first terms
                  # ngrams with contain the last terms and didn't exist in the above order
                  group<-group2gram[!(gsub(".* ","",group2gram$ngramtxt) %in% 
                                            gsub(".* ","",group3gram$ngramtxt)),]
                  alpha <-beta/sum((group$cGT)/Cden)
                  return(data.frame(Prob=alpha*Cnum/Cden, ngram=ngram[1], type="22",stringsAsFactors = FALSE))}
            
            #calc prob 1
            if(!is.na(pos[4])) {          # exists w ngram
                  Cnum<- CC$cGT[pos[4]]   # discounted freq of w ngram
                  Cden<- sum(CC$freq[CC$type==1])  # sum of all 1grams
                  beta <- CC$leftover[pos[4]]  #leftover of w ngram
                  #calculate alpha
                  group2gram <- CC[CC$type==1,] # ngrams which contain the first terms
                  group3gram <- CC[CC$nm1gram==ngram[6],] # ngrams which contain the first terms
                  # ngrams with contain the last terms and didn't exist in the above order
                  group<-group2gram[!(gsub(".* ","",group2gram$ngramtxt) %in% 
                                            gsub(".* ","",group3gram$ngramtxt)),]
                  alpha <-beta/sum((group$cGT)/Cden)
                  return(data.frame(Prob=alpha*Cnum/Cden, ngram=ngram[1], type="23",stringsAsFactors = FALSE))}
      }
      
      if(!is.na(pos[7])) {          # w-1,w-1 ngram found, we can continue
            #calc prob 4
            if(!is.na(pos[3])) {          # exists w-1,w ngram
                  Cnum<- CC$cGT[pos[3]]   # discounted freq of w-1,w ngram
                  Cden<- CC$freq[pos[7]]  # freq of w-1 ngram
                  return(data.frame(Prob=Cnum/Cden, ngram=ngram[1], type="31",stringsAsFactors = FALSE))}
            
            #calc prob 1
            if(!is.na(pos[4])) {          # exists w ngram
                  Cnum<- CC$cGT[pos[4]]   # discounted freq of w ngram
                  Cden<- sum(CC$freq[CC$type==1])  # sum of all 1grams
                  beta <- CC$leftover[pos[4]]  #leftover of w ngram
                  #calculate alpha
                  group2gram <- CC[CC$type==1,] # ngrams which contain the first terms
                  group3gram <- CC[CC$nm1gram==ngram[7],] # ngrams which contain the first terms
                  # ngrams with contain the last terms and didn't exist in the above order
                  group<-group2gram[!(gsub(".* ","",group2gram$ngramtxt) %in% 
                                            gsub(".* ","",group3gram$ngramtxt)),]
                  alpha <-beta/sum((group$cGT)/Cden)
                  return(data.frame(Prob=alpha*Cnum/Cden, ngram=ngram[1], type="32",stringsAsFactors = FALSE))}
      }
      if(is.na(pos[7])) {          # w-1,w-1 ngram found, we can continue
            #calc prob 4
            if(!is.na(pos[4])) {          # exists w-3,w ngram
                  Cnum<- CC$cGT[pos[4]]   # discounted freq of w-3,w ngram
                  Cden<- sum(CC$freq[CC$type==1])  # freq of w-3,w-1 ngram
                  return(data.frame(Prob=Cnum/Cden, ngram=ngram[1], type="41",stringsAsFactors = FALSE))}
      }
      return(0)
}


#OBSOLETE FUNCTIONS
library(tau)

IndexLevels <- function(txtv, v=vocab) {  # txtv = text vector  #### OBSOLETE
      #needs a character vector (vocab) with all the words in the document
      #returns a data frame, with the same number of columns as the ngram type
      #but with the words replaced by the word location in vocab
      foo <- data.frame(do.call('rbind', strsplit(as.character(txtv),' ',fixed=TRUE))) # create factor data.frame with tokens
      ncol=dim(foo)[2]
      for(i in 1:ncol) {
            levels(foo[,i])<-match(levels(foo[,i]),v)       # replace txt factors by integer factor
            foo[,i] <- as.integer(levels(foo[,i]))[foo[,i]] # replace factor by integer
      }
      foo
}

createCC<- function() { #### OBSOLETE
      load("In1_1.Rdata")
      N1<-temp2[,.N, by="freq"][order(freq)]
      load("In2_1.Rdata")
      N2<-temp2[,.N, by="freq"][order(freq)]
      load("In3_1.Rdata")
      N3<-temp2[,.N, by="freq"][order(freq)]
      load("In4_1.Rdata")
      N4<-temp2[,.N, by="freq"][order(freq)]
      
      k=5
      cGT<-data.table(f=0:k
                      ,N1=c_ast(N=N1,k=k,nWords=sum(N1$N),ngramtype=1)
                      ,N2=c_ast(N=N2,k=k,nWords=sum(N1$N),ngramtype=2)
                      ,N3=c_ast(N=N3,k=k,nWords=sum(N1$N),ngramtype=3)
                      ,N4=c_ast(N=N4,k=k,nWords=sum(N1$N),ngramtype=4))
      save(N1, N2, N3, N4, file="All_FreqOfFreq_N_tables.Rdata")
      rm(N1, N2, N3, N4)
      
      
      load("In4_1.Rdata")
      C4<-merge(temp2, cGT[,.(f,cGT=N4)], by.x = "freq", by.y="f", all.x = TRUE) # add cGT column
      C4[is.na(cGT), cGT:=as.numeric(freq)]                                      # add cGT column
      setkey(C4, X1, X2, X3, X4)
      # C4$type=4L  # add type column (optional)
      leftoverN3<-C4[ ,.(leftover=1-sum(cGT)/sum(freq)), by=list (X1, X2, X3)]
      setkey(leftoverN3, X1, X2, X3)
      # C4<-C4[C4$freq>1,]
      # C4$leftover<-NA # add type column (optional)
      
      load("In3_1.Rdata")
      C3<-merge(temp2, cGT[,.(f,cGT=N3)], by.x = "freq", by.y="f", all.x = TRUE) # add cGT column
      C3[is.na(cGT), cGT:=as.numeric(freq)]                                      # add cGT column
      setkey(C3, X1, X2, X3)
      # C3$type=3L
      leftoverN2<-C3[ ,.(leftover=1-sum(cGT)/sum(freq)), by=list (X1, X2)]
      setkey(leftoverN2, X1, X2)
      # C3<-C3[C3$freq>1,]
      C3<-merge(C3, leftoverN3, all.x = TRUE)
      
      load("In2_1.Rdata")
      C2<-merge(temp2, cGT[,.(f,cGT=N2)], by.x = "freq", by.y="f", all.x = TRUE) # add cGT column
      C2[is.na(cGT), cGT:=as.numeric(freq)]                                      # add cGT column
      setkey(C2, X1, X2)
      # C2$type=2L
      leftoverN1<-C2[ ,.(leftover=1-sum(cGT)/sum(freq)), by=list (X1)]
      setkey(leftoverN1, X1)
      # C2<-C2[C2$freq>1,]
      C2<-merge(C2, leftoverN2, all.x = TRUE)
      
      load("In1_1.Rdata")
      C1<-merge(temp2, cGT[,.(f,cGT=N1)], by.x = "freq", by.y="f", all.x = TRUE) # add cGT column
      C1[is.na(cGT), cGT:=as.numeric(freq)]                                      # add cGT column
      setkey(C1, X1)
      # C1$type=1L
      C1<-merge(C1, leftoverN1, all.x = TRUE)
      
      rm(leftoverN1, leftoverN2, leftoverN3, temp2, N1, N2, N3, N4)
}

createNgramIntIndex <- function(corpus=PC, n=2L) { #obsolete
      #      corpus <- PC   (for testing)
      ndocs=length(corpus)  # processes all documents in the corpora
      N<-list()
      for(j in 1:ndocs) {      #for testing, j=1; i=1
            nlines<- length(corpus[[j]][[1]])
            iterations=5+1 # process 5% at a time, shouldn't create problems
            Groups <-as.integer(seq(from=1, to=nlines, length=iterations))
            for(i in 1:(iterations-1)) {  # 1st cycle: write CSV with ngram count
                  print(paste("write",j,i,Sys.time()))
                  temp <- corpus[[j]][[1]][Groups[i]:(Groups[i+1]-1)]
                  temp <- textcnt(temp, n = n, split = "[[:space:]]+",
                                  method = "string",tolower = TRUE, marker = "_",
                                  recursive = FALSE, persistent = FALSE, useBytes = FALSE,
                                  perl = TRUE, verbose = FALSE, decreasing = FALSE)
                  temp <- temp[!grepl("\\beosent\\b", names(temp))] #removes all ngrams that contain "eos" marker
                  if(n==1) {
                        temp <- data.table(ngramtxt=names(temp),freq=as.integer(temp), key="ngramtxt")
                  } 
                  if (n>1) {
                        df<-IndexLevels(names(temp))
                        temp <- data.table(df,freq=as.integer(temp), key=names(df)[1:n])
                  }
                  file <- paste("ngram_n",n,"_",j,"_",i,".RData",sep="") 
                  save(temp, file = file)
            }
            
            file.remove(file) # already in memory
            temp2 <- temp   # 2nd cycle reuses last file and iterates downward
            
            for(i in (iterations-2):1) { #2nd cycle: read and merge CSVs
                  #i=19 ; j=1 ; n=2
                  print(paste("read",j,i,Sys.time()))
                  file <- paste("ngram_n",n,"_",j,"_",i,".Rdata",sep="")
                  load(file)   #restores the temp file that was saved in this file
                  temp2 <- merge(temp2, temp,
                                 all=TRUE, suffixes = c(".x", ".y"))
                  file.remove(file)
                  temp2[is.na(temp2)] <- 0
                  temp2[,freq:=as.integer(freq.x+freq.y)][,freq.x:=NULL][,freq.y:=NULL]
            }
            #            N[[j]]<-temp2[,.N, by="freq"][order(freq)]
            file <- paste("In",n,"_",j,".Rdata",sep="") 
            save(temp2, file = file)
            rm(temp, temp2, file)
            print(paste("done doc",j,Sys.time()))
      }
      #      N[[1]] #only working for if ndocs==1, otherwise must be calculated for merge of "temp2", outside this function
}

CreateNn<- function() {       #obsolete
      N1<- createNgramIntIndex(PCclean, n=1L)
      load("In1_1.Rdata")
      vocab<-temp2$ngramtxt
      temp2[,X1:=1:.N]
      save(temp2, file="In1_1.Rdata")
      rm(temp2)
      N2<- createNgramIntIndex(PCclean, n=2L)
      N3<- createNgramIntIndex(PCclean, n=3L)
      N4<- createNgramIntIndex(PCclean, n=4L)
}
