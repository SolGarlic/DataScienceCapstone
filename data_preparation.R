#####################################
### Modified Kneser–Ney smoothing _ ALT VERSION###
#####################################


## Auxiliary data creation. Need PCclean.

library("RWeka")
library(tokenizers)
library("tm")

multimatch<- function(DF=c(170693,27379), TB=temp2[,1:2], nomatch=NA, RightToLeft=FALSE) {
      # DF is a vector with  n values to be matched against the same columns of TB
      # TB is a table with same number of columns of DF, 
      #    it must be sorted from Left to Right or from Right to Left
      # the return is a two column DF in which the first column is the best match
      #    and the second column is the number of words matched
      n<-length(DF)
      si <- c(1:n)
      si<-sapply(1:n, function(i) sum(sum(TB[,i]==DF[[i]])))
#      for (i in 1:n) si[i]<-sum(TB[,i]==DF[[i]]) # number of matches
      
      if(RightToLeft) i<-c(n:1L)
      if(!RightToLeft) i<-c(1:n)
      m <- c(i)
      m[i[1]] <- match(DF[i[1]],TB[,i[1]])
      
      for (j in i[2:n]) {
            m[j] <- m[j-1]-1+match(DF[i[j]],TB[m[j-1]:m[j-1]+si[j-1],i[j]])
      }
#      m3 <- m2-1+match(DF[i[3]],TB[m2:m2+si[2],i[3]])
      return(c(m2,2))
}


indinv <- function(x,k=180000) {
      # x is the index of ngram with words indexed according to "vocab"
      # x = X1+X2*1e6+X3*1e12+X4*1e18... max n5, min n2
      n=6-match(TRUE, x %/% k^(4:1)>0) # n gram
      ind=c(1:n)
      for(i in (n):1) {
            ind[i] <- x %/% k^(i-1)
            x <- x %% k^(i-1)
      }
      ind
}
ind <- function(x=temp3[,1:4],k=180000) {
      # x dataframe with n columns containing word indexes from vocab
      n=dim(x)[2]
      z=c(1:dim(x)[1])
      for (i in 1:n) {
            z <- z + x[,i]*k^(i-1)
      }
      z
}

createDF <- function(rdata="ngram_n1_1_OK.Rdata", greater=0) {
      load(rdata)
      g1<-temp2$freq>greater
      freq <- temp2[g1,]
      ifelse(grepl(" ",temp2[1,1])
             ,freq <- data.frame(
                   do.call('rbind',strsplit(as.character(freq$ngramtxt)
                                            ,' ',fixed=TRUE)),
                   freq=freq$freq,stringsAsFactors = FALSE)
             ,names(freq)[1] <- "X1")
      row.names(freq) <- temp2$ngramtxt[g1]
      return(freq)
}

df1<-createDF("ngram_n1_1_OK.Rdata", 0)
df2<-createDF("ngram_n2_1_OK.Rdata", 0)
df3<-createDF("ngram_n3_1_OK.Rdata", 1)
df4<-createDF("ngram_n4_1_OK.Rdata", 1)

Dwi_N1plus <- table(df2[,2])

wim1D_N1 <- table(as.character(df2[df2$freq==1,][,1]))
wim1D_N2 <- table(as.character(df2[df2$freq==2,][,1]))
wim1D_N3plus <- table(as.character(df2[df2$freq>2,][,1]))

df3$X1X2 <- paste(df3$X1, df3$X2, sep=" ")

wim2im1D_N1 <- table(df3[df3$freq==1,][,5])
wim2im1D_N2 <- table(df3[df3$freq==2,][,5])
wim2im1D_N3plus <- table(df3[df3$freq>2,][,5])

df4$X1X2X3 <- paste(df4$X1, df4$X2, df4$X3, sep=" ")

wim3im1D_N1 <- table(df4[df4$freq==1,][,6])
wim3im1D_N2 <- table(df4[df4$freq==2,][,6])
wim3im1D_N3plus <- table(df4[df4$freq>2,][,6])

##################

dfn <- data.frame(n1g=as.numeric(table(df1$freq)[1:4]), 
                  n2g=as.numeric(table(df2$freq)[1:4]), 
                  n3g=as.numeric(table(df3$freq)[1:4]),
                  n4g=as.numeric(table(df4$freq)[1:4]))

PKNwi <- Dwi_N1plus/sum(Dwi_N1plus)

df4pruneP=NA
df3pruneP=NA
df2pruneP=NA
