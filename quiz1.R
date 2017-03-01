#1
file.size("./final/en_US/en_US.blogs.txt")/1024^2

#2
processFile = function(filepath) {
      con = file(filepath, "r")
      nl=0
      while ( TRUE ) {
            line = readLines(con, n = 1)
            
            if ( length(line) == 0 ) {
                  break
            }
            nl=nl+1
            #            print(line)
      }
      close(con)
      print(nl)
}
# processFile("./final/en_US/en_US.twitter.txt")

#3
processFileLL = function(filepath) {
      con = file(filepath, "r")
      ll=0
      while ( TRUE ) {
            line = readLines(con, n = 1)
            
            if ( length(line) == 0 ) {
                  break
            }
            ll <- max(ll, nchar(line))
            #            print(line)
      }
      close(con)
      print(ll)
}
# ll1 <- processFileLL("./final/en_US/en_US.twitter.txt")
# ll2 <- processFileLL("./final/en_US/en_US.blogs.txt")
# ll3 <- processFileLL("./final/en_US/en_US.news.txt")
# ll<- max(ll1,ll2,ll3)
# ll

#4
processFileLH = function(filepath) {
      con = file(filepath, "r")
      love=0
      hate=0
      while ( TRUE ) {
            line = readLines(con, n = 1)
            
            if ( length(line) == 0 ) {
                  break
            }
            love=love + grepl("love",line)
            hate=hate + grepl("hate",line)
            #            print(line)
      }
      close(con)
      print(love/hate)
}
# processFileLH("./final/en_US/en_US.twitter.txt")

#5
processFileBIO = function(filepath) {
      con = file(filepath, "r")
      love=0
      hate=0
      while ( TRUE ) {
            line = readLines(con, n = 1)
            
            if ( length(line) == 0 ) {
                  break
            }
            if(grepl("biostats",line)) {
                  print(line)
            }
      }
      close(con)
}
# processFileBIO("./final/en_US/en_US.twitter.txt")

#6
processFileK = function(filepath) {
      con = file(filepath, "r")
      sentence=0
      while ( TRUE ) {
            line = readLines(con, n = 1)
            
            if ( length(line) == 0 ) {
                  break
            }
            sentence=sentence + ("A computer once beat me at chess, but it was no match for me at kickboxing"==line)
            #            print(line)
      }
      close(con)
      print(sentence)
}
# processFileK("./final/en_US/en_US.twitter.txt")
