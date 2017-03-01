#####################################
### Modified Kneser–Ney smoothing ###
#####################################


## Auxiliary data creation. Need PCclean.

library("RWeka")
library(tokenizers)
library("tm")

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
tdm <- TermDocumentMatrix(PCclean, control = list(tokenize = BigramTokenizer))
freq4 <- rowSums(inspect(tdm))
#freq4 <- termFreq(PC[[1]], control = list(tokenize = BigramTokenizer))

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm <- TermDocumentMatrix(PCclean, control = list(tokenize = BigramTokenizer))
freq3 <- rowSums(inspect(tdm))

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(PCclean, control = list(tokenize = BigramTokenizer))
freq2 <- rowSums(inspect(tdm))

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
tdm <- TermDocumentMatrix(PCclean, control = list(tokenize = BigramTokenizer,
                                                  wordLengths=c(1,Inf)))
freq1 <- rowSums(inspect(tdm))

rm(tdm, BigramTokenizer)

df1 <- data.frame(X1=names(freq1), freq=freq1)
#row.names(df1) <- c()
rm(freq1)
# df1 <- data.frame(X1=freq1$ngramtxt, freq=freq1$freq) ; rm(freq1)

df2 <- strsplit(names(freq2)," ")
df2 <- unlist(df2)
df2 <- matrix(df2,ncol=2, byrow=T)
df2 <- data.frame(df2,freq=freq2, stringsAsFactors = FALSE)
#df2 <- data.frame(df2, stringsAsFactors = FALSE)
#df2$freq <- freq2
rm(freq2)

df3 <- strsplit(names(freq3)," ")
df3 <- unlist(df3)
df3 <- matrix(df3,ncol=3, byrow=T)
df3 <- data.frame(df3, freq=freq3, stringsAsFactors = FALSE)
#df3 <- data.frame(df3, stringsAsFactors = FALSE)
#df3$freq <- freq3
rm(freq3)

df4 <- strsplit(names(freq4)," ")
df4 <- unlist(df4)
df4 <- matrix(df4,ncol=4, byrow=T)
df4 <- data.frame(df4, freq=freq4, stringsAsFactors = FALSE)
#df4 <- data.frame(df4, stringsAsFactors = FALSE)
#df4$freq <- freq4
rm(freq4)


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

df2prune <- df2[df2$freq>1,]
df2pruneP <- apply(df2prune[,1:2], 1, function(x) PKN(as.character(x)))
df3prune <- df3[df3$freq>1,]
df3pruneP <- apply(df3prune[,1:3], 1, function(x) PKN(as.character(x)))
df4prune <- df4[df4$freq>1,]
df4pruneP <- apply(df4prune[,1:4], 1, function(x) PKN(as.character(x)))




# consolidate data (NEED TO CALCULATE PROBABILITIES)
a1=data.frame(wim3im1D_N1)
a2=data.frame(wim3im1D_N2)
a3=data.frame(wim3im1D_N3plus)
n3gram<-merge(a1,a2,by="Var1", all.x = TRUE, all.y=TRUE)
n3gram<-merge(n3gram, a3,by="Var1", all.x = TRUE, all.y=TRUE)
n3gram<-merge(n3gram, df3, by.x = "Var1", by.y = "row.names", all.x = TRUE, all.y=TRUE)
n3gram<- n3gram[,c(1:4,8)]
n3gram<-merge(n3gram, df3pruneP, by.x = "Var1", by.y = "row.names", all.x = TRUE, all.y=TRUE)
names(n3gram) <- c("name", "N1", "N2", "N3plus","freq","PKN")
n3gram$type <- 3L

a1=data.frame(wim2im1D_N1)
a2=data.frame(wim2im1D_N2)
a3=data.frame(wim2im1D_N3plus)
n2gram<-merge(a1,a2,by="Var1", all.x = TRUE, all.y=TRUE)
n2gram<-merge(n2gram, a3,by="Var1", all.x = TRUE, all.y=TRUE)
n2gram<-merge(n2gram, df2, by.x = "Var1", by.y = "row.names", all.x = TRUE, all.y=TRUE)
n2gram<- n2gram[,c(1:4,7)]
n2gram<-merge(n2gram, df2pruneP, by.x = "Var1", by.y = "row.names", all.x = TRUE, all.y=TRUE)
names(n2gram) <- c("name", "N1", "N2", "N3plus","freq","PKN")
n2gram$type <- 2L

a1=data.frame(wim1D_N1)
a2=data.frame(wim1D_N2)
a3=data.frame(wim1D_N3plus)
n1gram<-merge(a1,a2,by="Var1", all.x = TRUE, all.y=TRUE)
n1gram<-merge(n1gram, a3,by="Var1", all.x = TRUE, all.y=TRUE)
n1gram<-merge(n1gram, df1, by.x = "Var1", by.y = "row.names", all.x = TRUE, all.y=TRUE)
n1gram<- n1gram[,c(1:4,6)]
a1=data.frame(PKNwi)
n1gram<-merge(n1gram, a1, by = "Var1", all.x = TRUE, all.y=TRUE)
names(n1gram) <- c("name", "N1", "N2", "N3plus","freq","PKN")
rm(a1, a2, a3)
n1gram$type <- 1L

n4gram<-merge(df4, df4pruneP, by = "row.names", all.y=TRUE)[,c(1,6,8)]
names(n4gram) <- c("name","freq","PKN")
n4gram$N3plus <- n4gram$N2 <- n4gram$N1 <- NA
n4gram <- n4gram[,c(1,4:6,2,3)]
n4gram$type <- 4L

n14gram<- rbind(n1gram, n2gram, n3gram, n4gram)
n14gram$name <- as.character(n14gram$name)
n14gram$freq <- as.integer(n14gram$freq)
nprune <- n14gram[n14gram$freq>1,]
row.names(nprune) <- c()
np <- data.table(nprune)

# differences in length between all Ns are due to some words only 
#     appearing in th beginning or the end of sentences








# to get size of objects
sort( sapply(ls(),function(x){object.size(get(x))})) 

#time test #1: using 2 columns vs using name
system.time( replicate(100, temp<-df2[df2$X1=="zygote" & df2$X2=="no",] ) )
system.time( replicate(100, temp<-df2beta["zygote no",] ) )

#time test #1: using 2 columns vs using name
#system.time( replicate(100, temp<-sum(df2$X1=="zygote") ) )
#system.time( replicate(100, temp<-sum(grepl("^zygote",row.names(df2beta) ) ) ) )



Rprof("log.txt", interval = 0.01)
replicate(200, PKN(c("went","to","the","place")))
Rprof(NULL)
