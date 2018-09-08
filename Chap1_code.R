## Figure 1.1: PMF Plot for Counting with Replacement example. 
# Save plot as .eps file in your current working directory, and closing it using dev.off() at last.
# If you want to know your current working directory, you can run function "getwd()".
setEPS()
# Set file name "pmf_Count.eps", and width, height of the output figure
postscript("pmf_Count.eps",width = 5, height = 5)
# xx: the number of yellow draws in 10 draws;
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
# dhyper: probability mass funciton, 
plot(0:10, dhyper(x=0:10, m=10, n=10, k=10),main="PMF",xlab="x",ylab="p(x)", type="h", lwd=2)
points(0:10,dhyper(0:10, 10, 10, 10),col=2)
# TO GET THE CMF, WE NEED TO GET PARTIAL SUMS OF THE PMF.
plot(0:10, cumsum(dhyper(0:10, 10, 10, 10)),
     main="CDF",xlab="x",ylab="F(x)", type="s", lwd=2)
dev.off()

