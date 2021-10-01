### Functions for plotting pmf/cdf of discrete random 
### variables with integer range. 


plot_pmf = function(R, p){
  # A function to plot the pmf of a discrete random variable
  # with integer range. 
  #
  # Inputs: R is range of the random variable, p is the pmf
  plot(1, type = 'n', xlim = c(min(R) - 1, max(R) + 1), ylim = c(0, 1), xlab = 'x', 
         ylab = 'p(x)', main = 'Histogram', cex.lab = 1.3,
         cex.axis = 1.3, cex.main = 1.5, bty = 'n')
  
  n = length(R)
  for(x in 1:n){
    rect(R[x] - 0.5, 0, R[x] + 0.5, p[x], col = 'grey75', border = 'black', lwd = 2)
  }
}

### Test
R = c(1, 2, 3)
p = c(0.2, 0.7, 0.1)
plot_pmf(R, p)





plot_cdf = function(R, F){
  # A function to plot the cdf of a discrete random variable
  # with integer range. 
  #
  # Inputs: R is range of the random variable, F is the cdf
  #         R should be in increasing order
  n = length(R)
  plot(1, type = 'n', xlim = c(R[1] - 2, R[n] + 2), ylim = c(0, 1), xlab = 'x', 
       ylab = 'F(x)', main = 'Cumulative Distribution Function', cex.lab = 1.3,
       cex.axis = 1.3, cex.main = 1.5, bty = 'n')
  
  lines(c(R[1] - 2, R[1]), c(0, 0), lwd = 1.5)
  lines(c(R[1], R[1]), c(0, F[1]), lwd = 1.2, lty = 2)
  R[n + 1] = R[n] + 2
  for(x in 1:(n-1)){
    lines(c(R[x], R[x + 1]), c(F[x], F[x]), lwd = 1.5)
    lines(c(R[x+1], R[x+1]), c(F[x], F[x + 1]), lwd = 1.2, lty = 2)
  } 
  lines(c(R[n], R[n+1]), c(1, 1), lwd = 1.5)
  F = c(0, F)
  points(R[1:n], F[1:n], cex = 1.5, lwd = 2)
  points(R[1:n], F[2:(n+1)], cex = 1.5, lwd = 2, pch = 19)
}

# Test
R = c(1, 2, 3, 4)
F = c(0.1, 0.5, 0.7, 1)
plot_cdf(R, F)


R = c(0, 1, 2, 3)
p = c(1/20, 9/20, 9/20, 1/20)
F = c(1/20, 10/20, 19/20, 1)
# after running pmf and cdf plotting functions
plot_pmf(R, p)
plot_cdf(R, F)