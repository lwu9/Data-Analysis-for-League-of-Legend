# Hypergoemetric
setEPS()
postscript("pmfcdf.eps",width = 11, height = 6)
# THE FOLLOWING PUTS TWO GRAPHS ONE ON TOP OF THE OTHER ON EACH PAGE
par(mfrow = c(1, 2))
# NOW PLOT THE PMF. type="h" MEANS THAT THE PLOT IS VERTICAL LINES.
# points ADDS CIRCLES.
plot(0:10, dhyper(0:10, 10, 10, 10),main="PMF",xlab="x",ylab="p(x)", type="h", lwd=2)
points(0:10,dhyper(0:10, 10, 10, 10),col=2)
# TO GET THE CMF, WE NEED TO GET PARTIAL SUMS OF THE PMF.
plot(0:10, cumsum(dhyper(0:10, 10, 10, 10)),
     main="CDF",xlab="x",ylab="F(x)", type="s", lwd=2)
dev.off()

# Binomial
setEPS()
postscript("pmfcdf_bin.eps",width = 11, height = 6)
# THE FOLLOWING PUTS TWO GRAPHS ONE ON TOP OF THE OTHER ON EACH PAGE
par(mfrow = c(1, 2))
# NOW PLOT THE PMF. type="h" MEANS THAT THE PLOT IS VERTICAL LINES.
# points ADDS CIRCLES.
plot(0:20, dbinom(0:20, 20, 0.5),main="PMF",xlab="x",ylab="p(x)", type="h", lwd=2)
points(0:20,dbinom(0:20, 20, 0.5),col=2)
# TO GET THE CMF, WE NEED TO GET PARTIAL SUMS OF THE PMF.
plot(0:20, cumsum(dbinom(0:20, 20, 0.5)),
     main="CDF",xlab="x",ylab="F(x)", type="s", lwd=2)
dev.off()

# Counting with Replacement
setEPS()
postscript("pmfcdf_Count.eps",width = 11, height = 6)
# THE FOLLOWING PUTS TWO GRAPHS ONE ON TOP OF THE OTHER ON EACH PAGE
par(mfrow = c(1, 2))
# NOW PLOT THE PMF. type="h" MEANS THAT THE PLOT IS VERTICAL LINES.
# points ADDS CIRCLES.
xx = 0:10; count = factorial(10)/(factorial(xx)*factorial(10-xx))
plot(0:10, count/2^10, main="PMF",xlab="x",ylab="p(x)", type="h", lwd=2)
points(0:10, count/2^10,col=2)
# TO GET THE CMF, WE NEED TO GET PARTIAL SUMS OF THE PMF.
plot(0:10, cumsum(count/2^10),
     main="CDF",xlab="x",ylab="F(x)", type="s", lwd=2)
dev.off()

data = read.csv("https://github.com/DoransLab/data/raw/master/crit_smoothing/crits.csv")
head(data)
n = dim(data)[1]
nomial_p = (1:9)/10
# Assume independence for each trial, then we do binomial test
for (i in 1:9) {
  xx = sum(data[,i]) # total number of crits in ith column
  pp = nomial_p[i] # nomial probability of crits in ith column
  p_hat = xx/n
  if (p_hat > pp) alter = "greater" else  
    alter = "less"
  test = binom.test(x = xx, n, p = pp,
                    alternative = alter)
  print(test$p.value)
}
# Assume 1-order Markov
k = 1
for (i in 1:9){
  for (crit in 0:1) {
    nn =  sum((data[1:(n-k),i] == crit)) # the number of pairs where crit happens first 
    xx = sum((data[1:(n-k),i] == crit) & (data[(1+k):n,i] == 1))
    pp = nomial_p[i]
    p_hat = xx/nn
    if (p_hat > pp) alter = "greater" else  
      alter = "less"
    test = binom.test(x = xx, n = nn, p = pp,
                      alternative = alter)
    print(paste(i, crit,test$p.value))
  }
}

# Transition matrix for 1-order Markov Chain
for (i in 1:9) {
  tran_matrix = matrix(0,2,2)
  for (crit in 0:1) {
    nn =  sum((data[1:(n-k),i] == crit)) 
    xx = sum((data[1:(n-k),i] == crit) & (data[(1+k):n,i] == 0))
    tran_matrix[(crit+1),1] = xx/nn
    tran_matrix[(crit+1),2] = 1-xx/nn
  }
  print(i)
  print(tran_matrix)
}

# Transition matrix for 2-order Markov Chain
k = 2
for (i in 1:9) {
  tran_matrix = matrix(0,4,2)
  for (crit1 in 0:1) {
    for (crit2 in 0:1) {
      first_two = (data[1:(n-k),i] == crit1) & (data[k:(n-k+1),i] == crit2)
      third = (data[(1+k):n,i] == 0) & first_two
      j = crit1*2+(crit2+1)
      tran_matrix[j,1] = sum(third)/sum(first_two)
    }
  }
  tran_matrix[,2] = 1 - tran_matrix[,1] 
  print(i)
  print(tran_matrix)
}

# the length of runs
for (i in 1:9) {
  print(rle(data[,i]))
}
