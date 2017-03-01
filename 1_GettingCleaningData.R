
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
      mysel <- sapply(1:sel, function(i) scan(input, what="character", 
                                              skip=skip[i],nlines=1, 
                                              sep="\n", quiet=TRUE))
      close(input, "r")
      closeAllConnections()
      filename <- gsub(".txt","_S.txt",x,fixed=TRUE)
      filename <- gsub("en_US","en_US_TEST",filename,fixed=TRUE)
      write(unlist(mysel),file=filename)
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
      PC <- VCorpus(source,
                    readerControl = list(reader = readPlain,
                                         language = "en", load = TRUE))
      }
      
      if (!Virtual) {
      PC <- PCorpus(source,
                    readerControl = list(reader = readPlain,
                                         language = "en", load = TRUE),
                    dbControl = list(useDb = TRUE,
                                     dbName = "en_US.db",
                                     dbType = "DB1"))
      }
      return(PC)
}
           
#Clean final PC               
      
cleanAlt <- function() {      #Obsolete
      skipPunctuation <- function(x) removePunctuation(x,
                                                       preserve_intra_word_dashes = TRUE)
      transliterate <- content_transformer(function(x) {
            iconv(x, from="latin1", to="ASCII//TRANSLIT")
            iconv(x, "UTF-8", "ASCII",sub='')
      })
      profanity <- readLines("full-list-of-bad-words-banned-by-google-csv-file_2013_11_26_04_52_30_695.csv")
      skipWords <- function(x) removeWords(x, c(
            stopwords("en"),
            profanity))
      skipWords2 <- function(x) removeWords(x,
                                            c(profanity
                                              ,stopwords("en")
                                              ,"i","you","my","have","he","we","so","from","me","its"
                                              ,"all","said","his","your","just"
                                            ))
      funs <- list(stripWhitespace,
                   stemDocument,  #only after creating a StemCompletion dictionary
                   skipPunctuation,
#                   skipWords,
                   skipWords2,
                   removeNumbers,
                   transliterate,
                   content_transformer(tolower)
      )
      
      
      PCclean <- tm_map(PC, FUN = tm_reduce, tmFuns = funs)
}
      
############################
#  Clean PC ALTERNATIVE    #
############################


gsubCorpus <- function (corpus, pattern, replacement, fixed=FALSE) {
      tm_map(corpus,
             content_transformer(gsub), 
             pattern = pattern, 
             replacement = replacement, 
             fixed=fixed)
}

cleanCorpus <- function(PCclean) {
      library(tm)
      # replace most punctuation by " "
      PCclean <- gsubCorpus(PCclean, "[][|~<>_¢^\\}\\\\{()ºª£€§@<=>#$%*+/&·`´¨-]+", " ")
      # remove numbers (to also remove additionl ".,")
      PCclean <- gsubCorpus(PCclean, "\\d*(,|.)?\\d+", "")
      # replace strong punctuation by " eos ", including all 
      # punctuation that cause uncorrelation between words
      PCclean <- gsubCorpus(PCclean, "[!,.:;?]+", " eos ")
            # confirm if "," should be a sentence separator
      # transliterate
      transliterate <- content_transformer(function(x)
            iconv(x, from="latin1", to="ASCII//TRANSLIT"))
      # skip words (profanity, stopwords, additional words)
      profanity <- readLines("full-list-of-bad-words-banned-by-google-csv-file_2013_11_26_04_52_30_695.csv")
      mystopwords <- c("i","you","my","have","he","we","so","from","me","its"
                       ,"all","said","his","your","just")
      skipWords <- function(x) removeWords(x, c(
            stopwords("en"),
            mystopwords,
            profanity))
      skipPunctuation <- function(x) removePunctuation(x)
      # remove additional punctuation and foreign characters
      funs <- list(skipPunctuation,
                   transliterate             )
      PCclean <- tm_map(PCclean, FUN = tm_reduce, tmFuns = funs)
      
      # remove additional numbers, stopwords, whitespace, convert to lower and stem document
      funs <- list(removeNumbers,
                   skipWords,
                   stemDocument,
                   stripWhitespace,
                   content_transformer(tolower))
      PCclean <- tm_map(PCclean, FUN = tm_reduce, tmFuns = funs)
      return(PCclean)
}



chr_tokenizer <- function(x) unlist(strsplit(as.character(x), split=""))
library("RWeka")
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
ctrl <- list(tokenize = NGramTokenizer,
             wordLengths = c(1, Inf))
ctrl <- list(tokenize = chr_tokenizer,
             wordLengths = c(1, Inf))
freq1<-termFreq(PCclean[[1]], ctrl)
freq1 <- freq1[!grepl(" ?\\beof\\b ?", names(freq1))]
head(freq1,20)
cha<-sort(names(freq1))
head(cha,20)


x<-"André 1@£€] ¨ Abigail Reynolds   !#!$!#$#$%%&#<>$%^/}][6€§£ººº 33.44 3,1 João,.-()\\/(!(#$/)!(#*ªÇ)!(),.:;?|"
x4<-gsub("[!(),.:;?]+", "<EOF>",x2)
x2<-gsub(pattern="[][\\}{()ºª£€§@<=>#$%*+-/&']+", replacement=" ",x)
gsub(pattern="[][|<>^\\}{()ºª£€§@<=>#$%*+/&'-]+", replacement=" ",x)
x2<-gsub("\\d*(,|.)?\\d+", "",x3)
gsub("[[:punct:-[<>]]]", "",x2)
gsub("( <EOF> *)+", " <EOF> ",x3)


