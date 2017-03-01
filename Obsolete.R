
check <- function(x="./final/en_US/en_US.aaaaSUB.txt") {
      # just to check that the "SUB" character gives the error mentioned above
      input <- file(x, "r", encoding="ANSI_X3.4-1968") 
      remaining <- nlines(x)
      # get the records numbers to select 
      # compute number to skip on each read; account for the record just read 
      # allocate my data 
      mysel <- vector('character', remaining) 
      for (i in 1:remaining){
            d <- scan(input, what="character", 
                      nlines=10, allowEscapes = TRUE,
                      sep="\n", quiet=TRUE)
            #            if (identical (d,character(0))) { d <-""}
            ficheiro <- gsub(".txt","_SS.txt",x,fixed=TRUE)
            write(d,file=ficheiro, append=TRUE, sep="\n")
            #            mysel[i] <- d
      }
      close(input, "r")
      closeAllConnections()
      #      write(mysel,file=gsub(".txt","_S.txt",x,fixed=TRUE))
      #      mysel
}
