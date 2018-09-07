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

# the length of runs
for (i in 1:9) {
  print(rle(data[,i]))
}

