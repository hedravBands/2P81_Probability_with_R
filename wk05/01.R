#BINOMIAL DISTRIBUTION
#a
n <- 4
p <- 0.75  #success
ExpectedValue <- n*p
ExpectedValue
# = 3

#b p(X = 2)

choose(4,2)*p^2*(1-p)^2
# 0.2109

#c p(X>=2) = 1 - P(X=1) - p(X=0)
1 - choose(4,1)*p^1*(1-p)^3 - choose(4,0)*p^0*(1-p)^4
# alternative:  p(X=2) + p(X=3) + p(X=4)
choose(4,2)*p^2*(1-p)^2 + choose(4,3)*p^3*(1-p)^1 + choose(4,4)*p^4*(1-p)^0
#alternative: for loop with k iteration
seq <-c(2,3,4)
sum <- 0
for (k in seq){
  sum <- sum + choose(4,k)*p^k*(1-p)^(4-k)
}
sum
# 0.9492

