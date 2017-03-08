library(tm)
library(tau)
library(data.table)

createNgramIntIndex <- function(corpus=PC, n=2L) {
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

CreateNn<- function() {
      N1<- createNgramIntIndex(PCclean, n=1L, min=1)
            load("In1_1.Rdata")
            vocab<-temp2$ngramtxt
            temp2[,X1:=1:.N]
            save(temp2, file="In1_1.Rdata")
            rm(temp2)
      N2<- createNgramIntIndex(PCclean, n=2L, min=1)
      N3<- createNgramIntIndex(PCclean, n=3L, min=1)
      N4<- createNgramIntIndex(PCclean, n=4L, min=1)
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
IndexLevels <- function(txtv, v=vocab) {  # txtv = text vector
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

createCC<- function() {
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
      C4$type=4L  # add type column (optional)
      leftoverN3<-C4[ ,.(leftover=1-sum(cGT)/sum(freq)), by=list (X1, X2, X3)]
      setkey(leftoverN3, X1, X2, X3)
      C4<-C4[C4$freq>1,]
      C4$leftover<-NA # add type column (optional)
      
      load("In3_1.Rdata")
      C3<-merge(temp2, cGT[,.(f,cGT=N3)], by.x = "freq", by.y="f", all.x = TRUE) # add cGT column
      C3[is.na(cGT), cGT:=as.numeric(freq)]                                      # add cGT column
      setkey(C3, X1, X2, X3)
      C3$type=3L
      leftoverN2<-C3[ ,.(leftover=1-sum(cGT)/sum(freq)), by=list (X1, X2)]
      setkey(leftoverN2, X1, X2)
      C3<-C3[C3$freq>1,]
      C3<-merge(C3, leftoverN3, all.x = TRUE)

      load("In2_1.Rdata")
      C2<-merge(temp2, cGT[,.(f,cGT=N2)], by.x = "freq", by.y="f", all.x = TRUE) # add cGT column
      C2[is.na(cGT), cGT:=as.numeric(freq)]                                      # add cGT column
      setkey(C2, X1, X2)
      C2$type=2L
      leftoverN1<-C2[ ,.(leftover=1-sum(cGT)/sum(freq)), by=list (X1)]
      setkey(leftoverN1, X1)
      C2<-C2[C2$freq>1,]
      C2<-merge(C2, leftoverN2, all.x = TRUE)
      
      load("In1_1.Rdata")
      C1<-merge(temp2, cGT[,.(f,cGT=N1)], by.x = "freq", by.y="f", all.x = TRUE) # add cGT column
      C1[is.na(cGT), cGT:=as.numeric(freq)]                                      # add cGT column
      setkey(C1, X1)
      C1$type=1L
      C1<-merge(C1, leftoverN1, all.x = TRUE)
      
      rm(leftoverN1, leftoverN2, leftoverN3, temp2, N1, N2, N3, N4)
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

