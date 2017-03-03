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
                  temp <- temp[!grepl("\\beosent\\b", names(temp))] #removes all ngrams that contain "eos" marker
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

N1<- createNgram(PCclean, n=1L, min=1)
N2<- createNgram(PCclean, n=2L, min=1)
N3<- createNgram(PCclean, n=3L, min=1)
N4<- createNgram(PCclean, n=4L, min=1)

cGT<-data.frame(N1=c_ast(N=N1,k=5,nWords=sum(N1),ngramtype=1)
                ,N2=c_ast(N=N2,k=5,nWords=sum(N1),ngramtype=2)
                ,N3=c_ast(N=N3,k=5,nWords=sum(N1),ngramtype=3)
                ,N4=c_ast(N=N4,k=5,nWords=sum(N1),ngramtype=4))

calc_DiscoutF <- function(CC) {
      # takes a data frame with 2 columns (count (freq) and size of ngram (type))
      # returns returns a column with cGT
      # needs a cGT dataframe with the cGT for each freq and ngram size
      k<- dim(cGT)[1]-1
      DiscountFreq <- CC$freq
      for (i in range(CC$freq)[1]:k) {
            for (j in range(CC$type)[1]:range(CC$type)[2]) {
                  DiscountFreq[CC$type==j & CC$freq==i] <- cGT[i+1,j]
            }
      }
      DiscountFreq
}      
calc_leftOver <- function(CC) {
      # takes an ngram with at least 5 columns (ngramtxt, freq, nm1gram, type of ngram, cGT)
      # returns the table with the leftover probability for each nm1gram
      temp<-aggregate(CC[,c("freq","cGT")],list(CC$nm1gram),FUN="sum")
      temp$leftover<-(1-temp[,3]/temp[,2])
      leftoverprob<-temp[,c(1,4)]
      names(leftoverprob)<-c("ngramtxt", "leftover")
      leftoverprob
}

load("ngram_n4_1.Rdata")
C4<- temp2
C4$type=4
C4$cGT<-calc_DiscoutF(C4[,c("freq","type")])
leftoverN3<-calc_leftOver(C4)
C4<-C4[C4$freq>1,]
C4$leftover<-NA

load("ngram_n3_1.Rdata")
C3<-temp2
C3$type=3
C3$cGT<-calc_DiscoutF(C3[,c("freq","type")])
leftoverN2<-calc_leftOver(C3)
C3<-C3[C3$freq>1,]
C3<-merge(C3, leftoverN3, by="ngramtxt")

load("ngram_n2_1.Rdata")
C2<-temp2
C2$type=2
C2$cGT<-calc_DiscoutF(C2[,c("freq","type")])
leftoverN1<-calc_leftOver(C2)
C2<-C2[C2$freq>1,]
C2<-merge(C2, leftoverN2, by="ngramtxt")

load("ngram_n1_1.Rdata")
C1<-temp2
C1$nm1gram=""
C1$type=1
C1$cGT<-calc_DiscoutF(C1[,c("freq","type")])
C1<-merge(C1, leftoverN1, by="ngramtxt")

CC<-rbind(C1, C2, C3, C4)
rm(C1, C2, C3, C4, leftoverN1, leftoverN2, leftoverN3, temp)






GT_count <- function(count, nType=c(1:4)) {  #obsolete, not needed
      # this function returns the adjusted count based on the count and the ngram type
      # if count>k, returns the count
      # needs cGT data.frame as created above and shown below:
#      N1           N2           N3           N4
#      0       Inf 0.0003235196 1.928491e-08 6.770001e-13
#      1 0.3772528 0.1859409570 6.341883e-02 1.266002e-02
#
      if(count>=dim(cGT)[1]) return(count)
      return(cGT[count+1,nType])
}
      

CalcProb <- function(txt) {
      # returns the probability of the last word in the txt variable
      library(tm)
      # clean the text with the same procedure as the training corpus
      # and return the vector with the ngram
#      source <- VectorSource(txt)
#      PC2 <- VCorpus(source,
#                     readerControl = list(reader = readPlain,
#                                          language = "en", load = TRUE))
#      PC2 <- cleanCorpus(PC2)
#      ntxt<- unlist(strsplit(PC2[[1]][[1]][1]," "))
      ntxt<- unlist(strsplit(txt," "))
      n<- length(ntxt)
      ntxt<- ntxt[(n-3):n]

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
                  return(Prob=Cnum/Cden)}
            
            #calc prob 3
            if(!is.na(pos[2])) {          # exists w-2,w ngram
                  Cnum<- CC$cGT[pos[2]]   # discounted freq of w-2,w ngram
                  Cden<- CC$freq[pos[6]]  # freq of w-2,w-1 ngram
                  beta <- CC$leftover[pos[2]]  #leftover of w-2,w ngram
                  #calculate alpha
                  group2gram <- CC[CC$nm1gram==ngram[6],] # ngrams which contain the first terms
                  group3gram <- CC[CC$nm1gram==ngram[5],] # ngrams which contain the first terms
                  # ngrams with contain the last terms and didn't exist in the above order
                  group<-group2gram[gsub(".* ","",group2gram$ngramtxt) %in% 
                                          gsub(".* ","",group3gram$ngramtxt),]
                  alpha <-beta/sum((group$cGT)/Cden)
                  return(Prob=alpha*Cnum/Cden)}
            
            #calc prob 2
            if(!is.na(pos[3])) {          # exists w-1,w ngram
                  Cnum<- CC$cGT[pos[3]]   # discounted freq of w-1,w ngram
                  Cden<- CC$freq[pos[7]]  # freq of w-1,w-1 ngram
                  beta <- CC$leftover[pos[3]]  #leftover of w-1,w ngram
                  #calculate alpha
                  group2gram <- CC[CC$nm1gram==ngram[7],] # ngrams which contain the first terms
                  group3gram <- CC[CC$nm1gram==ngram[5],] # ngrams which contain the first terms
                  # ngrams with contain the last terms and didn't exist in the above order
                  group<-group2gram[gsub(".* ","",group2gram$ngramtxt) %in% 
                                          gsub(".* ","",group3gram$ngramtxt),]
                  alpha <-beta/sum((group$cGT)/Cden)
                  return(Prob=alpha*Cnum/Cden)}
      
            #calc prob 1
            if(!is.na(pos[4])) {          # exists w ngram
                  Cnum<- CC$cGT[pos[4]]   # discounted freq of w ngram
                  Cden<- sum(CC$freq[CC$type==1])  # sum of all 1grams
                  beta <- CC$leftover[pos[4]]  #leftover of w ngram
                  #calculate alpha
                  group2gram <- CC[CC$type==1,] # ngrams which contain the first terms
                  group3gram <- CC[CC$nm1gram==ngram[5],] # ngrams which contain the first terms
                  # ngrams with contain the last terms and didn't exist in the above order
                  group<-group2gram[gsub(".* ","",group2gram$ngramtxt) %in% 
                                          gsub(".* ","",group3gram$ngramtxt),]
                  alpha <-beta/sum((group$cGT)/Cden)
                  return(Prob=alpha*Cnum/Cden)}
      }
      if(!is.na(pos[6])) {          # w-2,w-1 ngram found, we can continue
            #calc prob 4
            if(!is.na(pos[2])) {          # exists w-3,w ngram
                  Cnum<- CC$cGT[pos[2]]   # discounted freq of w-3,w ngram
                  Cden<- CC$freq[pos[6]]  # freq of w-3,w-1 ngram
                  return(Prob=Cnum/Cden)}
            
            #calc prob 3
            if(!is.na(pos[3])) {          # exists w-2,w ngram
                  Cnum<- CC$cGT[pos[3]]   # discounted freq of w-2,w ngram
                  Cden<- CC$freq[pos[7]]  # freq of w-2,w-1 ngram
                  beta <- CC$leftover[pos[3]]  #leftover of w-2,w ngram
                  #calculate alpha
                  group2gram <- CC[CC$nm1gram==ngram[7],] # ngrams which contain the first terms
                  group3gram <- CC[CC$nm1gram==ngram[6],] # ngrams which contain the first terms
                  # ngrams with contain the last terms and didn't exist in the above order
                  group<-group2gram[gsub(".* ","",group2gram$ngramtxt) %in% 
                                          gsub(".* ","",group3gram$ngramtxt),]
                  alpha <-beta/sum((group$cGT)/Cden)
                  return(Prob=alpha*Cnum/Cden)}
            
            #calc prob 1
            if(!is.na(pos[4])) {          # exists w ngram
                  Cnum<- CC$cGT[pos[4]]   # discounted freq of w ngram
                  Cden<- sum(CC$freq[CC$type==1])  # sum of all 1grams
                  beta <- CC$leftover[pos[4]]  #leftover of w ngram
                  #calculate alpha
                  group2gram <- CC[CC$type==1,] # ngrams which contain the first terms
                  group3gram <- CC[CC$nm1gram==ngram[6],] # ngrams which contain the first terms
                  # ngrams with contain the last terms and didn't exist in the above order
                  group<-group2gram[gsub(".* ","",group2gram$ngramtxt) %in% 
                                          gsub(".* ","",group3gram$ngramtxt),]
                  alpha <-beta/sum((group$cGT)/Cden)
                  return(Prob=alpha*Cnum/Cden)}
      }
      
      if(!is.na(pos[7])) {          # w-1,w-1 ngram found, we can continue
            #calc prob 4
            if(!is.na(pos[3])) {          # exists w-3,w ngram
                  Cnum<- CC$cGT[pos[3]]   # discounted freq of w-3,w ngram
                  Cden<- CC$freq[pos[7]]  # freq of w-3,w-1 ngram
                  return(Prob=Cnum/Cden)}
            
            #calc prob 1
            if(!is.na(pos[4])) {          # exists w ngram
                  Cnum<- CC$cGT[pos[4]]   # discounted freq of w ngram
                  Cden<- sum(CC$freq[CC$type==1])  # sum of all 1grams
                  beta <- CC$leftover[pos[4]]  #leftover of w ngram
                  #calculate alpha
                  group2gram <- CC[CC$type==1,] # ngrams which contain the first terms
                  group3gram <- CC[CC$nm1gram==ngram[7],] # ngrams which contain the first terms
                  # ngrams with contain the last terms and didn't exist in the above order
                  group<-group2gram[gsub(".* ","",group2gram$ngramtxt) %in% 
                                          gsub(".* ","",group3gram$ngramtxt),]
                  alpha <-beta/sum((group$cGT)/Cden)
                  return(Prob=alpha*Cnum/Cden)}
      }
      if(is.na(pos[7])) {          # w-1,w-1 ngram found, we can continue
            #calc prob 4
            if(!is.na(pos[4])) {          # exists w-3,w ngram
                  Cnum<- CC$cGT[pos[4]]   # discounted freq of w-3,w ngram
                  Cden<- sum(CC$freq[CC$type==1])  # freq of w-3,w-1 ngram
                  return(Prob=Cnum/Cden)}
      }
      return(0)
}

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
