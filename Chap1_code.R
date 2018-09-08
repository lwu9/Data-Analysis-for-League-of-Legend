## Figure 1.1: PMF Plot for Counting with Replacement example. 
# Save plot as .eps file in your current working directory, and closing it using dev.off() at last.
# If you want to know your current working directory, you can run function "getwd()".
setEPS()
# Set file name "pmf_Count.eps", and width, height of the output figure
postscript("pmf_Count.eps",width = 5, height = 5)
# xx: a vector of the number of yellow draws what could happen in 10 draws;
# count: the number of possibilities for each corresponding xx;
# count/2^10: calculating the probability for each corresponding xx;
xx = 0:10; count = factorial(10)/(factorial(xx)*factorial(10-xx))
# Now plot the PMf. 
# type="h": means the plot is vertical lines.
# main, xlab, ylab: set the names of title, x axis and y axis
plot(0:10, count/2^10, main="Probability Mass Function",xlab="k",ylab="P(k yellow draws in 10 draws)", type="h", lwd=2)
# Add red circles on the plot.
points(0:10, count/2^10, col=2)
dev.off()

## Figure 1.3: PMF and CDF plots for Hypergoemetric distribution.
setEPS()
postscript("pmfcdf_HG.eps",width = 11, height = 6)
# The following puts two graphs side by side.
par(mfrow = c(1, 2))
# dhyper: probability mass funciton for Hypergoemetric distribution;
# x: a vector of the number of yellow balls what could happen in a draw of size k;
# m: the number of yellow balls in the bag;
# n: the number of red balls in the bag;
# k: the number of balls drawn from the bag.
plot(0:10, dhyper(x=0:10, m=10, n=10, k=10),main="PMF",xlab="x",ylab="p(x)", type="h", lwd=2)
points(0:10,dhyper(0:10, 10, 10, 10),col=2)
# To get the CMF, we need to get partial sums of the PMF, using function "cumsum".
# type="s": stair steps;
# lwd: the width of the line in the plot.
plot(0:10, cumsum(dhyper(0:10, 10, 10, 10)),
     main="CDF",xlab="x",ylab="F(x)", type="s", lwd=2)
dev.off()

##  Figure 1.4: PMF and CDF plots for Binomial distribution.
setEPS()
postscript("pmfcdf_bin.eps",width = 11, height = 6)
par(mfrow = c(1, 2))
# dbinom: probability mass funciton for Binomial distribution;
# x: a vector of the number of successes what could happen;
# size: the number of trials;
# prob: probability of success on each trial;
plot(0:20, dbinom(x=0:20, size=20, prob=0.5), main="PMF",xlab="x",ylab="p(x)", type="h", lwd=2)
points(0:20,dbinom(0:20, 20, 0.5),col=2)
plot(0:20, cumsum(dbinom(0:20, 20, 0.5)),
     main="CDF",xlab="x",ylab="F(x)", type="s", lwd=2)
dev.off()


## This part will calculate the Transition matrices using all the crit data
# Read data into R.
data = read.csv("https://github.com/DoransLab/data/raw/master/crit_smoothing/crits.csv")
# Take a look at the first 6 rows of data.
head(data)
# Check the dimensions of data. "dim" will tell you this data set has 2700 rows and 9 colums.
dim(data)

# Assign the number of rows in the data to "n". "n" means the number of attacks in one game
n = dim(data)[1]
# Using for-loop to calculate transition matrix for each column.
for (i in 1:p) {
  # Set a 2 by 2 matrix first
  tran_matrix = matrix(0,2,2)
  for (ct in 0:1) {
  # the firt attack in each pair could be 0 or 1, so we use for-loop, denote it as "ct".
    # nn: the total number of pairs where the first attack is "ct".
    # xx: the total number of pairs where the first attack is "ct and (i.e. "&") the second attack is Non-Crit.    
    nn =  sum((data[1:(n-1),i] == ct)) 
    xx = sum((data[1:(n-1),i] == ct) & (data[2:n,i] == 0))
    # Calculate the trainsition probability, and save it in the transition matrix
    tran_matrix[(ct+1),1] = xx/nn
    tran_matrix[(ct+1),2] = 1-xx/nn
  }
  # Print the transition matrix for ith column data after rounding 3 digital numbers 
  print(i)
  print(round(tran_matrix, 3))
}

