# modified from
# http://faculty.washington.edu/kenrice/sisg/SISG-08-06.pdf

carrier <- rep(c(0,1),c(100,200))
null.y <- rnorm(300)
alt.y <- rnorm(300, mean = carrier/2) #higher mean with carrier
t.test(null.y~carrier)
t.test(alt.y~carrier)
# calculates mean difference between case and control
null.diff <- mean(null.y[carrier==1])- mean(null.y[carrier==0])
alt.diff <- mean(alt.y[carrier==1]) - mean(null.y[carrier==0])
one.test <- function(x,y){
    xstar <- sample(x)
    #calculates mean difference between case and control
    mean(y[xstar==1]-mean(y[xstar==0]))
}

# carrier ie phenotype label is being permuted
nperm <- 10000
many.truenull <- replicate(nperm, one.test(carrier, null.y))
many.falsenull <- replicate(nperm, one.test(carrier, alt.y))

# plot histogram
par(mfrow=c(2,1))
hist(many.truenull)
abline(v = null.diff, col = "red")

hist(many.falsenull)
graph.diff = alt.diff
# draw in the lines if out of range
if(alt.diff>0.4){
    graph.diff = 0.4
}

abline(v = graph.diff, col = "red")


## Results
cat(nperm, " permutations \n")
cat("Empirical p null ",mean(abs(many.truenull) > abs(null.diff)),"\n")
cat("Empirical p alt ", mean(abs(many.falsenull) > abs(alt.diff)),"\n")