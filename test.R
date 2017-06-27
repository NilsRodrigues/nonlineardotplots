# imports
source("rootDotplot.R")



# generates random samples
randomSamples = function (count, mean) {
  samples = rnorm(n=count, mean=mean, sd=mean/2)
  return(samples)
}

# generate input data
set.seed(1234)
samples = randomSamples(10000, 20)
samples = sort(samples)

# root
if (T) {
  root = rootDotplot()
  root$shrinkRate = 0.4
  root$dynamicPlot(samples)
  root$plotToPdf(samples, "direct")
}

