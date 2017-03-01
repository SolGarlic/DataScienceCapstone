library(tokenizers)
library(RWeka)
ngram3t <- function(x) unlist(tokenize_ngrams(as.character(x), lowercase = TRUE, n = 3L, n_min = 3))
ctrl3 <- list(tokenize = ngram3t)
dtm3<-DocumentTermMatrix(PCclean, control = ctrl3)


ngram1t <- function(x) tokenize_ngrams(x[[1]], lowercase = TRUE, n = 1L, n_min = 1)
ctrl1 <- list(tokenize = words)
dtm1<-DocumentTermMatrix(PCclean, control = ctrl1)

tdm1<-TermDocumentMatrix(PC, control = ctrl)
dim(tdm1)

matrix <- inspect(tdm1)
matrix <- (cbind(matrix, rowSums(matrix)))
matrix <- matrix[order(-matrix[,4]),]
matrix <- cbind(matrix, cumsum(matrix[,4]))

nterms90pc <- min(which(matrix[,5]>0.9*sum(matrix[,4])))
MostFreqTerms <- rownames(matrix[1:nterms90pc,])


library(tokenizers)
n2gram <- function(s) {
      unlist(tokenize_ngrams(unlist(s),n=2L))
      }
ctrl <- list(removeNumbers=TRUE,
             tokenize = n2gram,
             removePunctuation=list(
                   preserve_intra_word_dashes = TRUE),
             wordLengths = c(3, Inf)
)
dtm2<-DocumentTermMatrix(PC, control = ctrl)

list1n <- tokenize_ngrams(PC[[1]][[1]], n=1L, simplify = TRUE)

list2n <- sapply(list1n, function(x) ngrams(x,2L))


tf<-termFreq(PC[[1]]$content,ctrl)




PC[[1]][1][200]

matrix <- inspect(tdm1)
matrix <- (cbind(matrix, rowSums(matrix)))
matrix <- matrix[order(-matrix[,4]),]
matrix <- cbind(matrix, cumsum(matrix[,4]))

#con <- file("./final/en_US/en_US.twitter.txt", "r") 
#a <- c(a,readLines(con, 2)) ## Read the first line of text 
#readLines(con, 5) ## Read in the next 5 lines of text 
#close(con) ## It's important to close the connection when you are done


diag2 <- function(x) {
      sapply(1:min(dim(x)), function(f) x[f,f])
      }


