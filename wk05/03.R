n <- 10

#a X >= 1
k <- 10
p <- 0.99
1 - choose(n,k)*p^k*(1-p)^(n-k)
# 0.0956

#b 
seq <- c(0,1,2,3,4)
p <- (0.10 + 0.05 + 0.01) #fail
sum <- 0
for (k in seq){
  sum <- sum + choose(n,k)*p^k*(1-p)^(n-k)
}
sum
# 0.9870

#c X = 1

print(expectedValue <- n*0.1)
# 1