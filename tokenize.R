# function to create a file with ngrams (independently of the size of the thing)
#library(tokenizers)
library(tm)
#library(RWeka)
library(tau)

createNgram <- function(corpus, n=2L, min=1L) {
      # function creates a ngram frequency table (data "temp2" written in the file "ngram_nN_j.Rdata")
      #     n=ngram type, j=document in corpora
      # columns: 1 - ngramtxt
      #          2 - ngram freq
      #          3 - ngramtxt without the last word n_minus1_gram
      # besides creating the mentioned file, also returns frequency of frequencies!!!!
      #      corpus <- PC   (for testing)
      # needs: a corpora "corpus", 
      #           the ngram length "n", 
      #           and the minimum count to consider in the frequency table "min"
      ndocs=length(corpus)  # processes all documents in the corpora
      for(j in 1:ndocs) {      #for testing, j=1; i=1
            nlines<- length(corpus[[j]][[1]])
            iterations=20+1 # process 5% at a time, shouldn't create problems
            Groups <-as.integer(seq(from=1, to=nlines, length=iterations))
            for(i in 1:(iterations-1)) {  # 1st cycle: write CSV with ngram count
                  print(paste("write",j,i,Sys.time()))
                  temp <- corpus[[j]][[1]][Groups[i]:(Groups[i+1]-1)]
                  temp <- textcnt(temp, n = n, split = "[[:space:]]+",
                                  method = "string",tolower = TRUE, marker = "_",
                                  recursive = FALSE, persistent = FALSE, useBytes = FALSE,
                                  perl = TRUE, verbose = FALSE, decreasing = FALSE)
                  temp <- temp[!grepl("\\beos\\b", names(temp))]
                  temp <- data.frame(ngramtxt=names(temp), freq=as.integer(temp))
                  file <- paste("ngram_n",n,"_",j,"_",i,".RData",sep="") 
                  save(temp, file = file)
            }
            
            file.remove(file) # already in memory
            temp2 <- temp   # 2nd cycle reuses last file and iterates downward
            
            for(i in (iterations-2):1) { #2nd cycle: read and merge CSVs
                  #i=17 ; j=1 ; n=2
                  print(paste("read",j,i,Sys.time()))
                  file <- paste("ngram_n",n,"_",j,"_",i,".Rdata",sep="")
                  load(file)   #restores the temp file that was saved in this file
                  temp2 <- merge(temp2, temp, by="ngramtxt", all=TRUE)
                  file.remove(file)
                  temp2 <- data.frame(ngramtxt=temp2[,1],
                                      freq=rowSums(temp2[,-1], na.rm = TRUE),
                                      stringsAsFactors = FALSE)
            }
            file <- paste("ngram_n",n,"_",j,".Rdata",sep="") 
            print(paste("done doc",j,Sys.time()))
            if(n>1) temp2$nm1gram <- gsub(" ([^ ])*$","",temp2$ngramtxt)
            N <- table(temp2[,2])
            temp2 <- temp2[temp2$freq >=min,]
            save(temp2, file = file)
            rm(temp, temp2, file)
      }
      N # this is only reliable for the last document in the corpora, has to be completed if corpora has more than 1 doc
}

c_ast <- function(N=N1, k=5, nWords=0,ngramtype=1) {
      # N<-N2; k=5; unigram=FALSE
      # creates a numeric vector of length k+1 (0:k) with the GoodTuring Smooth count
#      if (nWords==0) {N0 <- 0 ; c_ast0 <- 0}
#      if (nWords>0) {N0 <- nWords^ngramtype-nWords ; c_ast0 <- (0+1)*N[1]/N0}
      N0 <- nWords^ngramtype-nWords
      N=c(N0,N)
      Const<- (k+1)*N[k+1+1]/N[1+1]
      c<-0:k
      c_ast <- ((c+1)*N[c+1+1]/N[c+1]- c*Const )/(1-Const)
#      c_ast <- c(c_ast0, c_ast)
      names(c_ast) <- 0:k
      c_ast
}

N1<- createNgram(PCclean, n=1L, min=2)
N2<- createNgram(PCclean, n=2L, min=2)
N3<- createNgram(PCclean, n=3L, min=2)
N4<- createNgram(PCclean, n=4L, min=2)

load("ngram_n4_1.Rdata")
C4 <- temp2
load("ngram_n3_1.Rdata")
C3 <- temp2
load("ngram_n2_1.Rdata")
C2 <- temp2
load("ngram_n1_1.Rdata")
C1 <- temp2
C1$nm1gram=""
C1$type=1
C2$type=2
C3$type=3
C4$type=4
CC<-rbind(C1, C2, C3, C4)


cGT<-data.frame(N1=c_ast(N=N1,k=5,nWords=sum(N1),ngramtype=1)
               ,N2=c_ast(N=N2,k=5,nWords=sum(N1),ngramtype=2)
               ,N3=c_ast(N=N3,k=5,nWords=sum(N1),ngramtype=3)
               ,N4=c_ast(N=N4,k=5,nWords=sum(N1),ngramtype=4)
               )
GT_count <- function(count, nType=c(1:4)) {
      # this function returns the adjusted count based on the count and the ngram type
      # if count>k, returns the count
      # needs cGT data.frame as created above and shown below:
#      N1           N2           N3           N4
#      0       Inf 0.0003235196 1.928491e-08 6.770001e-13
#      1 0.3772528 0.1859409570 6.341883e-02 1.266002e-02
#
      if(count>=dim(cGT)[1]) return(count)
      else return(cGT[count+1,nType])
}
      

CalcProb <- function(txt) {
      # returns the probability of the last word in the txt variable
      library(tm)
      # clean the text with the same procedure as the training corpus
      # and return the vector with the ngram
      source <- VectorSource(txt)
      PC2 <- VCorpus(source,
                     readerControl = list(reader = readPlain,
                                          language = "en", load = TRUE))
      PC2 <- cleanCorpus(PC2)
      ntxt<- unlist(strsplit(PC2[[1]][[1]][1])," ")
      n<- length(ntxt)
      ntxt<- ntxt[ntxt-3:ntxt]

      #calc prob 4
      n4gram <- paste(ntxt[1:4], collapse=" ")
      nm1gram <- paste(ntxt[1:3], collapse=" ")
      Cnum<- CC$freq[match(n4gram,CC$ngramtxt)]
      Cden<- CC$freq[match(n4gram,CC$ngramtxt)]
      Cnum<- GT_count(Cnum,4)
      Prob=Cnum/Cden
      
      
      
      
      
}

LeftOver <- function(txt) {
      nwords = 1+lengths(regmatches(txt, gregexpr(" ", txt)))
      if(nwords==3) # 1-sum(d(wwi)*c(wwi)/c(wwi-1) = 1-sum(GT_count(c(wwi)))/c(wwi-1) 
            {leftOverProb <- 1- sum(
                  sapply(C4[C4$nm1gram==txt,]$freq,function(x) GT_count(x,3))/
                        C3[C3$ngramtxt==txt,]$freq
            )}
      if(nwords==2) # 1-sum(d(wwi)*c(wwi)/c(wwi-1) = 1-sum(GT_count(c(wwi)))/c(wwi-1)
            {leftOverProb <- 1- sum(
                  sapply(C3[C3$nm1gram==txt,]$freq,function(x) GT_count(x,2))/
                        C2[C2$ngramtxt==txt,]$freq
            )}
      if(nwords==1) # 1-sum(d(wwi)*c(wwi)/c(wwi-1) = 1-sum(GT_count(c(wwi)))/c(wwi-1)
            {leftOverProb <- 1- sum(
                  sapply(C2[C2$nm1gram==txt,]$freq,function(x) GT_count(x,1))/
                        C1[C1$ngramtxt==txt,]$freq
            )}
      leftOverProb
}
LeftOverN3 <- sapply(C3$ngramtxt[1:2], LeftOver)


createNgramIntIndex <- function(corpus=PC, n=2L) {
      #      corpus <- PC   (for testing)
      ndocs=length(corpus)  # processes all documents in the corpora
      for(j in 1:ndocs) {      #for testing, j=1; i=1
            nlines<- length(corpus[[j]][[1]])
            iterations=20+1 # process 5% at a time, shouldn't create problems
            Groups <-as.integer(seq(from=1, to=nlines, length=iterations))
            for(i in 1:(iterations-1)) {  # 1st cycle: write CSV with ngram count
                  print(paste("write",j,i,Sys.time()))
                  temp <- corpus[[j]][[1]][Groups[i]:(Groups[i+1]-1)]
                  temp <- textcnt(temp, n = n, split = "[[:space:]]+",
                        method = "string",tolower = TRUE, marker = "_",
                        recursive = FALSE, persistent = FALSE, useBytes = FALSE,
                        perl = TRUE, verbose = FALSE, decreasing = FALSE)
                  df<-IndexLevels(names(temp))
                  temp <- data.frame(df,freq=as.integer(temp))
#                  temp <- data.frame(ngramtxt=names(temp), freq=as.integer(temp))
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
                  temp2 <- merge(temp2, temp, by=c(names(temp))[1:n], all=TRUE)
                  file.remove(file)
                  temp2 <- data.frame(temp2[,1:n], freq=rowSums(temp2[,-(1:n)], na.rm = TRUE))
            }
            file <- paste("ngram_n",n,"_",j,".Rdata",sep="") 
            save(temp2, file = file)
            rm(temp, temp2, file)
            print(paste("done doc",j,Sys.time()))
      }
}


IndexLevels <- function(txtv, v=vocab) {  # txtv = text vector
      #needs a character vector (vocab) with all the words in the document
      #returns a data frame, with the same number of columns as the ngram type
      #but with the words replaced by the word location in vocab
      foo <- data.frame(do.call('rbind', strsplit(as.character(txtv),' ',fixed=TRUE))) # create factor data.frame with tokens
      ncol=dim(foo)[2]
      for(i in 1:ncol) {
            levels(foo[,i])<-match(levels(foo[,i]),v)       # replace txt factors by integer factor
            foo[,i] <- as.numeric(levels(foo[,i]))[foo[,i]] # replace factor by integer
      }
      foo
}

ReverseIndexLevels <- function(df, v=vocab) { 
      ncol=dim(df)[2]
      for(i in 1:ncol) {
            df[,i]<-vocab[df[,i]]
      }
      foo<-apply(df, 2, function(x) paste(x, collapse=" "))
      foo
}
