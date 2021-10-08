# 

# a)
1 - exp(-50*2/60)
1 - dpois(0, lambda = 50*2/60)
#0.8111

# b)

L = 50*2/60
(dpois(1, lambda = L) + dpois(2, lambda = L)) / (1 - dpois(0, lambda = L))