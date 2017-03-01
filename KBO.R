#####################################
### Katz Back Off model           ###
#####################################


## Auxiliary data creation. Need PCclean.

library("RWeka")
library(tokenizers)
library("tm")

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
tdm <- TermDocumentMatrix(PCclean, control = list(tokenize = BigramTokenizer,
                                                  wordLengths=c(1,Inf)))
freq1 <- rowSums(inspect(tdm))
Nr1 <- data.frame(table(freq1))
names(Nr1) <- c("r", "Nr")

ggplot(data=Nr1, aes(y=Nr, x=r))+
      geom_point() +
      coord_trans(y = "log", x="log")+
      geom_smooth(method = "lm")

Nr1
