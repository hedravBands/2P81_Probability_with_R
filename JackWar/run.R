# JackWar Data Generator


# Global variables
max_tries   <- 1000
total_win   <- 0
total_lose  <- 0
total_tie   <- 0
total_war   <- 0
total_nowar <- 0
total_win2  <- 0
total_lose2 <- 0
total_tie2  <- 0


# Declare my original bet
myInitialBet <- 2

# Declare my original tie-bet
myOriginalTieBet <- 0

# Balance:
myGrandBalance <- 1000

# Strategy
onTieDifference <- 8

###############

# Util functions

newDeck <- function(){
  # face values: 2:10, J, K, Q, A  times 4 = 52 cards
  deck = c(2:10, 10, 10, 10, 11, 2:10, 10, 10, 10, 11, 2:10, 10, 10, 10, 11, 2:10, 10, 10, 10, 11) 
  
  return(deck)
}

# Draw card at any time during a game
drawCard <- function(){
  index = sample(1:52, 1)
  # re-sample until card is valid (value not 0)
  while (deck[index] == 0) {
    index <- sample(1:52, 1)
  }
  card <- deck[index]
  deck[index] <- 0 #set card as drawn (value 0)
  
  return(card)
}

# Decision-Maker 1: Absolute difference less than or equal to onTieDifference 
shouldBetOnTie <- function(h, d){
  return((abs(h[1]-d[1]) <= onTieDifference))
}

# Decision-Maker 2: Player's first card is greater than or equal to Dealer's
shouldDoubleBet <- function(h, d){
  return((h[1] - d[1]) >= 0)
}

# Decision-Maker 3: Decision whether or not go to WAR: DD => no war
shouldGoToWar <- function(dd){
  return(!dd)
}


# CHECK VICTORY FOR 1ST ROUND, 1 WIN, 0 TIE, -1 LOSE
isThisAVictory <- function(h, d){
  if ((h[1]+h[2]) - (d[1]+d[2]) > 0) {return(1)}
  if ((h[1]+h[2]) - (d[1]+d[2]) == 0) {return(0)}
  if ((h[1]+h[2]) - (d[1]+d[2]) < 0) {return(-1)}
}
##################################

## PLAY SEVERAL TIMES
for(i in 1:max_tries){
  

# MONEY RELATED
myBalance <- myOriginalBalance
myBet <- myInitialBet
myTieBet <- myOriginalTieBet

# GET INITIAL CARDS
hand <- c(drawCard(),0)
dealer <- c(drawCard(),0)

# DECISION-MAKER 1: BET ON TIE?
isBetOnTie = shouldBetOnTie(hand, dealer)
if (isBetOnTie) {myTieBet <- myBet}

# DECISION-MAKER 2: DOUBLE ORIGINAL BET?
isDoubleBet <- shouldDoubleBet(hand, dealer)
if (isDoubleBet) {myBet <- 2*myBet}

# Play for ROUND 1
hand[2] <- drawCard()
dealer[2] <- drawCard()

# DECISION-MAKER 3: GO TO WAR? Strategy: DD => NO WAR
isThisWar <- shouldGoToWar(isDoubleBet)

result <- isThisAVictory(hand, dealer)
results <- c(1:20)*0  #histogram using indexes where 10 is even, 11 is 1x gain, 1 is -1x loss

#  EVALUATE EACH POSSIBLE RESULT: WIN, LOSE, TIE, NO_WAR, WAR: WIN2, LOSE2, TIE2
if (result == 1) {
  total_win <- total_win + 1
  myBalance <- myBet*2 - myTieBet
  myGrandBalance <- myGrandBalance + myBalance
  index <- 10 + myBalance
  results[index] <- results[index] + 1
}

if (result == -1){
  total_lose <- total_lose + 1
  myBalance <- result*(myBet + myTieBet)
  myGrandBalance <- myGrandBalance + myBalance
  index <- (-1)*myBalance
  results[index] <- results[index] + 1
} 

if ((result == 0) & isBetOnTie) {
  total_tie <- total_tie + 1
  myBalance <- myTieBet*10 - myBet
  myGrandBalance <- myGrandBalance + myBalance
  index <- 10 + myBalance
  results[index] <- results[index] + 1
} 

if ((result == 0) & !isThisWar) { 
  total_nowar <- total_nowar + 1
  myBalance <- (-1/2)*myBet
  myGrandBalance <- myGrandBalance + myBalance
  index <- (-1)*myBalance
  results[index] <- results[index] + 1
  
} 

if  ((result == 0) & isThisWar) {
  total_war <- total_war + 1
  myBet <- 2*myBet
  
  # GET ANOTHER PAIR OF CARDS (AND DISCART THE OLD PAIR)
  hand <- c(drawCard(),drawCard())
  dealer <- c(drawCard(),drawCard())
  
  # GET NEW RESULT AND ANALYSE
  result <- isThisAVictory(hand, dealer)
  
  # ANALYSIS FOR THE SECOND ROUND, AFTER WAR
  if (result == 1) {
    total_win2 <- total_win2 + 1
    myBalance <- myBet
    myGrandBalance <- myGrandBalance + myBalance
    index <- 10 + myBalance
    results[index] <- results[index] + 1
    
  } else if (result == -1){
    total_lose2 <- total_lose2 + 1
    myBalance <- result*(myBet)
    myGrandBalance <- myGrandBalance + myBalance
    index <- (-1)*myBalance
    results[index] <- results[index] + 1
    
  } else  {
    total_tie2 <- total_tie2 + 1
    myBalance <- myBet*2
    myGrandBalance <- myGrandBalance + myBalance
    index <- 10 + myBalance
    results[index] <- results[index] + 1
  }
  
}
} # end for loop

#hist(results)
total_win  
total_lose  
total_tie  
total_war   
total_noWar 
total_win2  
total_lose2 
total_tie2  
myGrandBalance
results

# 
# > #hist(results)
#   > total_win  
# [1] 480
# > total_lose  
# [1] 435
# > total_tie  
# [1] 85
# > total_war   
# [1] 30
# > total_noWar 
# [1] 0
# > total_win2  
# [1] 14
# > total_lose2 
# [1] 15
# > total_tie2  
# [1] 1
# > myGrandBalance
# [1] 2930
# > results
# [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0



