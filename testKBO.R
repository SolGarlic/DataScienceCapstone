CalcProbALT("The guy in front of me just bought a pound of bacon, a bouquet, and a case of", c("beer", "cheese", "pretzels", "soda"))
CalcProbALT("You're the reason why I smile everyday. Can you follow me please? It would mean the", c("world", "most", "universe", "best"))
CalcProbALT("Hey sunshine, can you follow me and make me the", c("happiest", "saddest", "bluest", "smelliest"))
CalcProbALT("Very early observations on the Bills game: Offense still struggling but the", c("defense", "players", "crowd", "referees"))
CalcProbALT("Go on a romantic date at the",c("beach","mall", "movies", "grocery"))
CalcProbALT("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", c("way", "motorcycle", "phone", "horse"))
CalcProbALT("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", c("time", "weeks", "years", "thing"))
CalcProbALT("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", c("fingers", "eyes", "ears", "toes"))
CalcProbALT("Be grateful for the good times and keep the faith during the", c("bad", "sad", "worst", "hard"))
CalcProbALT("If this isn't the cutest thing you've ever seen, then you must be", c("insane", "callous", "insensitive", "asleep"))

CalcProbALT("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd", c("die", "eat", "sleep", "give"))
CalcProbALT("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his", c("marital", "spiritual", "financial", "horticultural"))
CalcProbALT("I'd give anything to see arctic monkeys this", c("weekend", "month", "morning", "decade"))
CalcProbALT("Talking to your mom has the same effect as a hug and helps reduce your", c("stress", "sleepiness", "hunger", "happiness"))
CalcProbALT("When you were in Holland you were like 1 inch away from me but you hadn't time to take a", c("walk", "look", "picture", "minute"))
CalcProbALT("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the", c("incident", "matter", "account", "case"))
CalcProbALT("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each", c("hand", "arm", "finger", "toe"))
CalcProbALT("Every inch of you is perfect from the bottom to the", c("top", "side", "center", "middle"))
CalcProbALT("Im thankful my childhood was filled with imagination and bruises from playing", c("outside", "inside", "daily", "weekly"))
CalcProbALT("I like how the same people are in almost all of Adam Sandler's", c("movies", "novels", "stories", "pictures"))


## OBSOLETE BELOW!!!      
      



c<-list(1:10)
c[[1]]<-rbind(
      CalcProb("The guy in front of me just bought a pound of bacon, a bouquet, and a case of beer")
      ,CalcProb("The guy in front of me just bought a pound of bacon, a bouquet, and a case of cheese")
      ,CalcProb("The guy in front of me just bought a pound of bacon, a bouquet, and a case of pretzels")
      ,CalcProb("The guy in front of me just bought a pound of bacon, a bouquet, and a case of soda")
)
c[[2]]<-rbind(
      CalcProb("You're the reason why I smile everyday. Can you follow me please? It would mean the world")
      ,CalcProb("You're the reason why I smile everyday. Can you follow me please? It would mean the most")
      ,CalcProb("You're the reason why I smile everyday. Can you follow me please? It would mean the universe")
      ,CalcProb("You're the reason why I smile everyday. Can you follow me please? It would mean the best")
)
c[[3]]<-rbind(
      CalcProb("Hey sunshine, can you follow me and make me the happiest")
      ,CalcProb("Hey sunshine, can you follow me and make me the saddest")
      ,CalcProb("Hey sunshine, can you follow me and make me the bluest")
      ,CalcProb("Hey sunshine, can you follow me and make me the smelliest")
)
c[[4]]<-rbind(
      CalcProb("Very early observations on the Bills game: Offense still struggling but the defense")
      ,CalcProb("Very early observations on the Bills game: Offense still struggling but the crowd")
      ,CalcProb("Very early observations on the Bills game: Offense still struggling but the referees")
      ,CalcProb("Very early observations on the Bills game: Offense still struggling but the players")
)
c[[5]]<-rbind(
      CalcProb("Go on a romantic date at the beach")
      ,CalcProb("Go on a romantic date at the mall")
      ,CalcProb("Go on a romantic date at the movies")
      ,CalcProb("Go on a romantic date at the grocery")
)
c[[6]]<-rbind(
      CalcProb("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my way")
      ,CalcProb("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my motorcycle")
      ,CalcProb("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my phone")
      ,CalcProb("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my horse")
)
c[[7]]<-rbind(
      CalcProb("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some time")
      ,CalcProb("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some weeks")
      ,CalcProb("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some years")
      ,CalcProb("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some thing")
)
c[[8]]<-rbind(
      CalcProb("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little fingers")
      ,CalcProb("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little eyes")
      ,CalcProb("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little ears")
      ,CalcProb("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little toes")
)
c[[9]]<-rbind(
      CalcProb("Be grateful for the good times and keep the faith during the bad")
      ,CalcProb("Be grateful for the good times and keep the faith during the sad")
      ,CalcProb("Be grateful for the good times and keep the faith during the worst")
      ,CalcProb("Be grateful for the good times and keep the faith during the hard")
)
c[[10]]<-rbind(
      CalcProb("If this isn't the cutest thing you've ever seen, then you must be insane")
      ,CalcProb("If this isn't the cutest thing you've ever seen, then you must be callous")
      ,CalcProb("If this isn't the cutest thing you've ever seen, then you must be insensitive")
      ,CalcProb("If this isn't the cutest thing you've ever seen, then you must be asleep")
)

lapply(1:10,function(x) c[[x]][,])
sapply(1:10,function(x) which.max(c[[x]][,1]))

c<-list(1:10)
c[[1]]<-rbind(
      CalcProb("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd die")
      ,CalcProb("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd eat")
      ,CalcProb("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd sleep")
      ,CalcProb("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd give")
)
c[[2]]<-rbind(
      CalcProb("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his marital")
      ,CalcProb("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his spiritual")
      ,CalcProb("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his financial")
      ,CalcProb("Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his horticultural")
)
c[[3]]<-rbind(
      CalcProb("I'd give anything to see arctic monkeys this weekend")
      ,CalcProb("I'd give anything to see arctic monkeys this month")
      ,CalcProb("I'd give anything to see arctic monkeys this morning")
      ,CalcProb("I'd give anything to see arctic monkeys this decade")
)
c[[4]]<-rbind(
      CalcProb("Talking to your mom has the same effect as a hug and helps reduce your stress")
      ,CalcProb("Talking to your mom has the same effect as a hug and helps reduce your sleepiness")
      ,CalcProb("Talking to your mom has the same effect as a hug and helps reduce your hunger")
      ,CalcProb("Talking to your mom has the same effect as a hug and helps reduce your happiness")
)
c[[5]]<-rbind(
      CalcProb("When you were in Holland you were like 1 inch away from me but you hadn't time to take a walk")
      ,CalcProb("When you were in Holland you were like 1 inch away from me but you hadn't time to take a look")
      ,CalcProb("When you were in Holland you were like 1 inch away from me but you hadn't time to take a picture")
      ,CalcProb("When you were in Holland you were like 1 inch away from me but you hadn't time to take a minute")
)
c[[6]]<-rbind(
      CalcProb("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the incident")
      ,CalcProb("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the matter")
      ,CalcProb("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the account")
      ,CalcProb("I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the case")
)
c[[7]]<-rbind(
      CalcProb("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each hand")
      ,CalcProb("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each arm")
      ,CalcProb("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each finger")
      ,CalcProb("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each toe")
)
c[[8]]<-rbind(
      CalcProb("Every inch of you is perfect from the bottom to the top")
      ,CalcProb("Every inch of you is perfect from the bottom to the side")
      ,CalcProb("Every inch of you is perfect from the bottom to the center")
      ,CalcProb("Every inch of you is perfect from the bottom to the middle")
)
c[[9]]<-rbind(
      CalcProb("Im thankful my childhood was filled with imagination and bruises from playing outside")
      ,CalcProb("Im thankful my childhood was filled with imagination and bruises from playing inside")
      ,CalcProb("Im thankful my childhood was filled with imagination and bruises from playing daily")
      ,CalcProb("Im thankful my childhood was filled with imagination and bruises from playing weekly")
)
c[[10]]<-rbind(
      CalcProb("I like how the same people are in almost all of Adam Sandler's movies")
      ,CalcProb("I like how the same people are in almost all of Adam Sandler's novels")
      ,CalcProb("I like how the same people are in almost all of Adam Sandler's stories")
      ,CalcProb("I like how the same people are in almost all of Adam Sandler's pictures")
)

lapply(1:10,function(x) c[[x]][,])
sapply(1:10,function(x) which.max(c[[x]][,1]))
