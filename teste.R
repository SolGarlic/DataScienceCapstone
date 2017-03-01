prepareTXT <- function(txt, nWords=4) {
      # given a sentence (txt) returns a clean vector of the last N tokens
      source <- VectorSource(txt)
      PC2 <- VCorpus(source,
                    readerControl = list(reader = readPlain,
                                         language = "en", load = TRUE))
      skipPunctuation <- function(x) {
            removePunctuation(x,preserve_intra_word_dashes = TRUE)}
      transliterate <- content_transformer(function(x) 
            iconv(x, from="latin1", to="ASCII//TRANSLIT"))
      profanity <- readLines("full-list-of-bad-words-banned-by-google-csv-file_2013_11_26_04_52_30_695.csv")
      # mySetOfStopWords <- c("a","and","for","i","in","is","it",
      #                      "of","on","that","the","to","was","with","you")
      skipWords <- function(x) removeWords(x, c(
            mySetOfStopWords, 
            profanity))
      funs <- list(stripWhitespace,
                   stemDocument,  #only after creating a StemCompletion dictionary
                   skipPunctuation,  #try and replace for
                   # gsub("[[:punct:]]* *(\\w+[&'-]\\w+)|[[:punct:]]+ *| {2,}", " \\1", x)
                   #should remove punctuation except apostrophe and dashes between words
                   skipWords,
                   removeNumbers,
                   #             transliterate,
                   content_transformer(tolower))
      PC2 <- tm_map(PC2, FUN = tm_reduce, tmFuns = funs)
      txt<-PC2[[1]][[1]]
      txt<- unlist(strsplit(txt, " "))
      n<-length(txt)
      txt3 <- txt[(n-nWords+1):n]
      return(txt3)
}


PKN(prepareTXT("The guy in front of me just bought a pound of bacon, a bouquet, and a case of beer"))
PKN(prepareTXT("The guy in front of me just bought a pound of bacon, a bouquet, and a case of cheese"))
PKN(prepareTXT("The guy in front of me just bought a pound of bacon, a bouquet, and a case of pretzels"))
PKN(prepareTXT("The guy in front of me just bought a pound of bacon, a bouquet, and a case of soda"))
PKN(prepareTXT("You're the reason why I smile everyday. Can you follow me please? It would mean the world"))
PKN(prepareTXT("You're the reason why I smile everyday. Can you follow me please? It would mean the most"))
PKN(prepareTXT("You're the reason why I smile everyday. Can you follow me please? It would mean the universe"))
PKN(prepareTXT("You're the reason why I smile everyday. Can you follow me please? It would mean the best"))
PKN(prepareTXT("Hey sunshine, can you follow me and make me the happiest"))
PKN(prepareTXT("Hey sunshine, can you follow me and make me the saddest"))
PKN(prepareTXT("Hey sunshine, can you follow me and make me the bluest"))
PKN(prepareTXT("Hey sunshine, can you follow me and make me the smelliest"))
PKN(prepareTXT("Very early observations on the Bills game: Offense still struggling but the crowd"))
PKN(prepareTXT("Very early observations on the Bills game: Offense still struggling but the referees"))
PKN(prepareTXT("Very early observations on the Bills game: Offense still struggling but the defense"))
PKN(prepareTXT("Very early observations on the Bills game: Offense still struggling but the players"))
PKN(prepareTXT("Go on a romantic date at the beach"))
PKN(prepareTXT("Go on a romantic date at the mall"))
PKN(prepareTXT("Go on a romantic date at the movies"))
PKN(prepareTXT("Go on a romantic date at the grocery"))
PKN(prepareTXT("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my way"))
PKN(prepareTXT("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my motorcycle"))
PKN(prepareTXT("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my phone"))
PKN(prepareTXT("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my horse"))
PKN(prepareTXT("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some time"))
PKN(prepareTXT("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some weeks"))
PKN(prepareTXT("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some years"))
PKN(prepareTXT("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some thing"))
PKN(prepareTXT("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little eyes"))
PKN(prepareTXT("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little ears"))
PKN(prepareTXT("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little fingers"))
PKN(prepareTXT("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little toes"))
PKN(prepareTXT("Be grateful for the good times and keep the faith during the bad"))
PKN(prepareTXT("Be grateful for the good times and keep the faith during the sad"))
PKN(prepareTXT("Be grateful for the good times and keep the faith during the worst"))
PKN(prepareTXT("Be grateful for the good times and keep the faith during the hard"))
PKN(prepareTXT("If this isn't the cutest thing you've ever seen, then you must be insane"))
PKN(prepareTXT("If this isn't the cutest thing you've ever seen, then you must be callous"))
PKN(prepareTXT("If this isn't the cutest thing you've ever seen, then you must be insensitive"))
PKN(prepareTXT("If this isn't the cutest thing you've ever seen, then you must be asleep"))
