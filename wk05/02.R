#a X = 5
n <- 5
p <- 0.6  #qualifies
k <- 5
choose(n,k)*p^k*(1-p)^(n-k)
# 0.0778

#b X >= 4
seq <-c(4,5)
sum <- 0
for (k in seq){
  sum <- sum + choose(n,k)*p^k*(1-p)^(n-k)
}
sum
# 0.3370