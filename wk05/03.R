n <- 10

#a X >= 1
k <- 10
p <- 0.99
1 - choose(n,k)*p^k*(1-p)^(n-k)
# 0.0956

#b X < 5 Fail  or X >= 5 success
seq <- c(5,6,7,8,9,10)
p <- 1 - (0.10 + 0.05 + 0.01) #success
sum <- 0
for (k in seq){
  sum <- sum + choose(n,k)*p^k*(1-p)^(n-k)
}
sum
# 0.9870

#c X = 1
p1<- 0.1
print(expectedValue <- n*p1)
# 1