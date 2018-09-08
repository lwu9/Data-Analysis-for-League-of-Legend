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
postscript("pmfcdf_Count.eps",width = 5, height = 5)
# NOW PLOT THE PMF. type="h" MEANS THAT THE PLOT IS VERTICAL LINES.
# points ADDS CIRCLES.
xx = 0:10; count = factorial(10)/(factorial(xx)*factorial(10-xx))
plot(0:10, count/2^10, main="Probability Mass Function",xlab="k",ylab="P(k yellow draws in 10 draws)", type="h", lwd=2)
points(0:10, count/2^10, col=2)
dev.off()


# Plots of comparing variances of two random varibles
setEPS()
postscript("vars.eps",width = 11, height = 6)
# THE FOLLOWING PUTS TWO GRAPHS ONE ON TOP OF THE OTHER ON EACH PAGE
par(mfrow = c(1, 2))
# NOW PLOT THE PMF. type="h" MEANS THAT THE PLOT IS VERTICAL LINES.
pp = c(1:10,11,10:1); prob=pp/sum(pp)
x1 = c(-(10:1),0:10)*2; x2 = x1/2
plot(x1, prob, main="PMF",xlab="x",ylab="p(x)", 
     type="h", lwd=2)
plot(x2, prob, main="PMF",xlab="x",ylab="p(x)", 
    xlim=c(-20,20),type="h", lwd=2)
dev.off()
# Calculate variances for the above two random variables
var1 = sum((x1 - sum(x1*prob))^2*prob) # 80
var2 = sum((x2 - sum(x2*prob))^2*prob) # 20

data = read.csv("https://github.com/DoransLab/data/raw/master/crit_smoothing/crits.csv")
head(data)
n = dim(data)[1]
nomial_p = (1:9)/10
# Assume independence for each trial, then we do binomial test
for (i in 1:9) {
  xx = sum(data[,i]) # total number of crits in ith column
  pp = nomial_p[i] # nomial probability of crits in ith column
  p_hat = xx/n
  test = binom.test(x = xx, n, p = pp,
                    alternative = "two.sided")
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
    test = binom.test(x = xx, n = nn, p = pp,
                      alternative = "two.sided")
    print(paste(i, crit,test$p.value))
  }
}

# Hypothesis test after considering 1-order Markov
for(i in c(1:4,6:7)) {
  for (crit in 0:1) {
    nn =  sum((data[1:(n-k),i] == crit)) # the number of pairs where crit happens first 
    xx = sum((data[1:(n-k),i] == crit) & (data[(1+k):n,i] == 1))
    p_hat = xx/nn
    for (pp in nomial_p) {
      test = binom.test(x = xx, n = nn, p = pp,
                        alternative = "two.sided")
      if (test$p.value > 0.05 & pp*10!=i)
      print(paste(i, crit,pp,test$p.value)) 
    }
  }
}

# Autocorrelations plots
for (i in 1:9) {
  setEPS()
  postscript(paste0("pmfcdf_acf",i,".eps"),width = 5, height = 5)
  acf(data[,i], ylab ="Autocorrelation Function", main=paste0("Nominal crit chance = ",i/10))
  dev.off()
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
  print(round(tran_matrix, 3))
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
  print(i)
  print(rle(data[,i]))
}

# Correlation plots
data2 = read.csv("/Users/lili/Downloads/match_player_corr_data.csv")
pairs(data2)
cor(data2) 
library(ggplot2)
setEPS()
postscript("corr1.eps",width = 6, height = 6)
ggplot(data2, aes(data2[,"gold_per_minute"], data2[,"deaths_per_minute"])) +
  geom_point() + theme_grey(base_size = 18)+
  xlab("Gold per minute") + ylab("Deaths per minute")
dev.off()
setEPS()
postscript("corr2.eps",width = 6, height = 6)
ggplot(data2, aes(data2[,"goldearned"], data2[,"totaldamagedealt"])) +
  geom_point() + theme_grey(base_size = 18) +
  xlab("Gold Earned") + ylab("Total Damage Dealt")
dev.off()
setEPS()
postscript("corr3.eps",width = 6, height = 6)
ggplot(data2, aes(data2[,"kills"], data2[,"deaths"])) +
  geom_point() + theme_grey(base_size = 18)+
  xlab("Kills") + ylab("Deaths")
dev.off()