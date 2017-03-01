
PKN <- function(txt=c("the","end","of","the")) {
      # txt should be a character vector with 1-4 terms
      # last term is wi
      n <- length(txt)
      txt2=txt[1:n-1]
      ngram <- paste(txt, collapse=" ")
      ngramn1 <- paste(txt2, collapse=" ")

      if(n==4) {
            P <- df4pruneP[ngram]
            if (!is.na(P)) return(P)
            sum <- df3[ngramn1,"freq"]
            ifelse(is.na(sum),
                   return(PKN(txt[2:n])), {
                               C   <- df4[ngram,"freq"]
                               if(is.na(C)) C<-0
                               D   <- D(C,n)
                               gama <- gama(txt[1:(n-1)])
                               P<-(C-D(C,n))/sum+gama*PKN(txt[2:n])}
                         )
      }
      if(n==3) {
            P <- df3pruneP[ngram]
            if (!is.na(P)) return(P)
            sum <- df2[ngramn1,"freq"]
            ifelse(is.na(sum),
                         return(PKN(txt[2:n])), {
                               C   <- df2[ngram,"freq"]
                               if(is.na(C)) C<-0
                               D   <- D(C,n)
                               gama <- gama(txt[1:(n-1)])
                               P<-(C-D(C,n))/sum+gama*PKN(txt[2:n])}
                        )
      }
      if(n==2) {
            P <- df2pruneP[ngram]
            if (!is.na(P)) return(P)
            sum <- df1[ngramn1,"freq"]
            ifelse(is.na(sum),
                         return(PKN(txt[2:n])), {
                               C   <- df2[ngram,"freq"]
                               if(is.na(C)) C<-0
                               D   <- D(C,n)
                               gama <- gama(txt[1:(n-1)])
                               P<-(C-D(C,n))/sum+gama*PKN(txt[2:n])}
            )
      }
      if(n==1) {
            ifelse(is.na(PKNwi[ngram]), 
                   P <- 1/sum(Dwi_N1plus),
                   P <- PKNwi[ngram])
      }
      P
}



#cwi3i <- df4[df4$X1==w3 & df4$X2==w2 & df4$X3==w1 & df4$X4==wi,5]
#      sumicwi3i <- sum(df4$freq[df4$X1==w3 & df4$X2==w2 & df4$X3==w1])
#sum <- df3$freq[ngramn1]
#D(cwi3i,4)


## rever construção das variáveis para facilitar os cálculos e acelerar as procuras
## manter nomes das linhas deve acelerar muito!!!

#PKNwi_c <- (cwi3i-D(cwi3i,4))/sumicwi3i





PKN_alt <- function(txt=c("the","end","of","the")) {
      # txt should be a character vector with 1-4 terms
      # last term is wi
      n <- length(txt)
      ngram     <- paste(txt, collapse=" ")
      
      dfng <- nprune[1:3,]
      
      i1<-tryCatch(which(nprune$name==ngram), error=function(e) NA)
      ifelse(length(i1)==0,
             dfng[1,] <- NA,
             dfng[1,] <- nprune[i1,])
      
      if(n>1) {
            txt2n     <- txt[2:n]
            ngram2n   <- paste(txt2n, collapse=" ")
            txt1nm1   <- txt[1:n-1]
            ngram1nm1 <- paste(txt1nm1, collapse=" ")
            
            i2<-tryCatch(which(nprune$name==ngram2n), error=function(e) NA)
            i3<-tryCatch(which(nprune$name==ngram1nm1), error=function(e) NA)
            # i3 is also the index for gama_alt function
            ifelse(length(i2)==0,
                   dfng[2,] <- NA,
                   dfng[2,] <- nprune[i2,])
            ifelse(length(i3)==0,
                   dfng[3,] <- NA,
                   dfng[3,] <- nprune[i3,])
            
            P <- dfng[1,]$PKN
            if (!is.na(P)) return(P)
            sum <- dfng[3,]$freq
            ifelse(is.na(sum), {
                  PKN <- dfng[2,]$PKN
                  ifelse(is.na(PKN), return(PKN_alt(txt2n)), return(PKN))
            },
            {C   <- dfng[1,]$freq
            if(is.na(C)) C<-0
            sum <- dfng[3,]$freq
            D   <- D(C,dfng[1,]$type)
            gama <- gama_alt(i3)
            P<-(C-D(C,n))/dfng[3,]$freq+gama*PKN(txt2n)}
            )
      }
      if(n==1) {
            ifelse(is.na(dfng[1,]$PKN), 
                   P <- 1/sum(Dwi_N1plus),
                   P <- dfng[1,]$PKN)
      }
      P
}



#gama
gama <- function(txt) {
      # txt should be a character vector with 1-4 terms
      #needs dfn as defined above (dataframe with count of n)
      n <- length(txt)
      ngram <- paste(txt, collapse=" ")
      if (n==3) {
            N1 <- wim3im1D_N1[ngram]
            N2 <- wim3im1D_N2[ngram]
            N3 <- wim3im1D_N3plus[ngram]
            denominator <- df3[ngram,"freq"]
      }
      if (n==2) {
            N1 <- wim2im1D_N1[ngram]
            N2 <- wim2im1D_N2[ngram]
            N3 <- wim2im1D_N3plus[ngram]
            denominator <- df2[ngram,"freq"]
      }
      if (n==1) {
            N1 <- wim1D_N1[ngram]
            N2 <- wim1D_N2[ngram]
            N3 <- wim1D_N3plus[ngram]
            denominator <- df1[ngram,"freq"]
      }
      
      #gama calculation
      (D(1,n) * ifelse(is.na(N1), 0, N1)+
                  D(2,n) * ifelse(is.na(N2), 0, N2)+
                  D(3,n) * ifelse(is.na(N3), 0, N3))/denominator
}

#gama
gama_alt <- function(index) {
      # index should be the position of the character vector with 1-4 terms
      # in nprune
      # needs dfn as defined above (dataframe with count of n)
      ng <- nprune[index,]
      ng[is.na(ng)] <- 0
      #gama calculation
      (D(1,ng$type) * ng$N1+
                  D(2,ng$type) * ng$N2 +
                  D(3,ng$type) * ng$N3plus)/ng$freq
}


D <- function(count , nwords=c(1:4)) {
      # calculates D depending on:
      #    - the count of the ngram(0, 1, 2, more than 3)
      #    - the type of ngram involved, 2-gram, 3-gram, 4-gram
      #          - it must exist a dataframe with the frequency of frequencies 
      #                 of the n-grams (frequencies from 1 to 4 in rows)
      #                 and the type of ngram (n2g, n3g, n4g) in columns)
      n1=dfn[1,nwords] ; n2=dfn[2, nwords]  ; n3=dfn[3,nwords] ; n4=dfn[4,nwords]
      if(count==0) {D <- 0}
      if(count==1) {D <- 1-2*(n1/(n1+2*n2)*n2/n1)}
      if(count==2) {D <- 2-3*(n1/(n1+2*n2)*n3/n2)}
      if(count>=3) {D <- 3-4*(n1/(n1+2*n2)*n4/n3)}
      D
}




