
library(tm)
library(tokenizers)



dlFile <- function() {
      temp <- tempfile()
      download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",temp)
      unzip(temp)
      unlink(temp)
}
# Control codes that prevent file loading (specially SUB) will be removed with NOTEPAD++, by replacing:
# the REGEX [^\x0A-\x0D\x1F-\x7F\xA0-\xFF]+
# this removes all control characters except line feeds.
# +90 substitutions in blogs.txt
# +27038 substitutions in news.txt
# +267 substitutions in twitter.txt
# +1M substitutions in all files!
# (based in http://www.aivosto.com/vbtips/control-characters.html)





# sample: sample 5% of each input file and create new files
sampleDIR <- function(dir="./final/en_US/", sample=0.4) {
      # checks a folder and calls the sampFILE function for each file
      filenames <- list.files(dir, pattern="*.txt", full.names=TRUE)
      ldf <- lapply(filenames, function(x) sampFILE(x, sample) )
}

sampFILE <- function(x="./final/en_US/en_US.news.txt", samplesize=0.1) {
      # samples 3000 (or half) of the lines of a text document and writes a smaller file.
      # NOTE: news.txt had a "SUB" character that made the scan stop (the first at line 77259, just after "At the Spice Merchant, a 2-ounce bag of tea leaves capable of producing 1"
      # It was manually removed with Notepad++
      input <- file(x, "r") 
      remaining <- nlines(x)
      sel <- as.integer(remaining * samplesize)
      # get the records numbers to select 
      set.seed(12345)
      recs <- sort(sample(1:remaining, sel)) 
      # compute number to skip on each read; account for the record just read 
      skip <- diff(c(0, recs)) - 1 
      # allocate my data 
      int<-20 #intervals
      sel <- as.integer(seq(1,sel, length.out = int+1)) # 10 intervals
      for (j in 1:int) {
            mysel <- sapply(sel[j]:sel[j+1], function(i) scan(input, what="character", 
                                                    skip=skip[i],nlines=1, 
                                                    sep="\n", quiet=TRUE))
            filename <- gsub(".txt",paste(j,".txt",sep=""),x,fixed=TRUE)
            filename <- gsub("en_US","en_US_TEST",filename,fixed=TRUE)
            write(unlist(mysel),file=filename)
      }
      close(input, "r")
      closeAllConnections()
      #      mysel
}

nlines <- function(x) {
      # finds the number of lines of a file. Used by sampFILE
      f <- file(x, open="rb")
      n <- 0L
      while (length(chunk <- readBin(f, "raw", 65536)) > 0) {
            n <- n + sum(chunk == as.raw(10L))
      }
      close(f)
      n
}



createPC <- function(Virtual=TRUE) {
      # Create Corpus #
      library(tm)
      source <- DirSource(directory = "./final/en_US_TEST/",
                          encoding = "UTF-8")
      
      if (Virtual) {
      PC <<- VCorpus(source,
                    readerControl = list(reader = readPlain,
                                         language = "en", load = TRUE))
      }
      
      if (!Virtual) {
      PC <<- PCorpus(source,
                    readerControl = list(reader = readPlain,
                                         language = "en", load = TRUE),
                    dbControl = list(useDb = TRUE,
                                     dbName = "en_US.db",
                                     dbType = "DB1"))
      }
#      return(PC)
}
           
StemCompleteDic <- function(PC) {
      # Creates a sorted text vector with the unstemmd version of the stemmed words
      # StemDictionary is a word list ordered by frequency
      # use: tm::stemCompletion(x, StemDictionary, type="first") to complete words
      # receives an almost clean Corpus (just before stemming)
      
      
      # skipPunctuation <- function(x) removePunctuation(x)
      # transliterate <- content_transformer(function(x) {
      #       iconv(x, from="latin1", to="ASCII//TRANSLIT")
      # })
      # profanity <- readLines("full-list-of-bad-words-banned-by-google-csv-file_2013_11_26_04_52_30_695.csv")
      # skipWords <- function(x) removeWords(x, c(
      #       stopwords("en"),
      #       profanity))
      # funs <- list(removeNumbers,
      #              skipWords,
      #              transliterate,
      #              skipPunctuation,
      #              stripWhitespace,
      #              content_transformer(tolower)
      # )
      # PCclean2 <- tm_map(PC, FUN = tm_reduce, tmFuns = funs)

      BigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))
      tdm <- TermDocumentMatrix(PC, control = list(tokenize = BigramTokenizer,
                                                        wordLengths=c(1,Inf)))

      stem<-data.table(token=tdm$dimnames$Terms, freq=as.integer(slam::row_sums(tdm, na.rm = T)), key="freq")
      
      StemDictionary<- stem[,changed:=token!=stemDocument(token)
                            ][changed==TRUE][order(-freq)
                                             ][, c("changed", "freq"):=NULL]
      save (StemDictionary, file="StemDictionary.Rdata")
      return(StemDictionary)
}
      
      
      #  Clean PC ALTERNATIVE    #


gsubCorpus <- function (corpus, pattern, replacement, fixed=FALSE) {
      tm_map(corpus,
             content_transformer(gsub), 
             pattern = pattern, 
             replacement = replacement, 
             fixed=fixed)
}

cleanCorpus <- function(PCclean, StemDic=FALSE) {
      # takes a gross Corpus and returns a clean one
      # use with             PCclean <- cleanCorpus(PC)
      library(tm)
      # replace most punctuation by " "   NOTA!!! these are temporarily removed from the pattern below: ´`¨§ºª¢£€·
      PCclean <- gsubCorpus(PCclean, "[][|~<>_,^\\}\\\\{)(<=>#$%*+/&@-]+", " ")  
      # remove numbers (to also remove additionl ".,")
      PCclean <- gsubCorpus(PCclean, "\\d*(,|.)?\\d+", "")
      # replace strong punctuation by " eosent ", including all 
      # punctuation that cause uncorrelation between words
      PCclean <- gsubCorpus(PCclean, "[!.:;?]+", " eosent ")
            # confirm if "," should be a sentence separator
      # transliterate
      transliterate <- content_transformer(function(x)
            iconv(x, from="latin1", to="ASCII//TRANSLIT"))
      # skip words (profanity, stopwords, additional words)
      profanity <- readLines("full-list-of-bad-words-banned-by-google-csv-file_2013_11_26_04_52_30_695.csv")
      mystopwords <- c("i","you","my","have","he","we","so","from","me","its"
                       ,"all","said","his","your","just")
      skipWords <- function(x) removeWords(x, c(
            tm::stopwords("en"),
#            mystopwords,
            profanity))
      skipPunctuation <- function(x) removePunctuation(x)
      # remove additional punctuation and foreign characters
      funs <- list(skipPunctuation,
                   transliterate)
      PCclean <- tm_map(PCclean, FUN = tm_reduce, tmFuns = funs)
      
      # remove additional numbers, stopwords, whitespace, convert to lower and stem document
      funs <- list(removeNumbers,
                   skipWords,
#                   stemDocument,
                   stripWhitespace,
                   content_transformer(tolower))
      PCclean <- tm_map(PCclean, FUN = tm_reduce, tmFuns = funs)

      if(StemDic) StemDictionary <<- StemCompleteDic(PCclean)

      funs <- list(stemDocument)
      PCclean <- tm_map(PCclean, FUN = tm_reduce, tmFuns = funs)
      
      return(PCclean)
}


CodeToRun <- function() {
      sampleDIR(sample=0.6)
      createPC(Virtual = FALSE)
      # PCclean <- cleanCorpus(PC, StemDic=TRUE)
      cleanCorpus(PC, StemDic=TRUE)
      
      
      
      
      
      
}