# imports
source("rootDotplot.R")
source("logDotplot.R")

# generate input data
set.seed(1234)
samples = rnorm(n=10000, mean=20, sd=2)
samples = sort(samples)

# plot KDE of sample data
if (F) {
  # check if package is installed
  prerequisites = c("ks")
  missingPrerequisites = prerequisites[!(prerequisites %in% installed.packages()[, "Package"])]
  if (length(missingPrerequisites))
    install.packages(missingPrerequisites)

  # load package
  library("ks")

  # plot sample distribution
  plot(kde(samples, h=0.05))
}


# root
if (T) {
  plot = rootDotplot()
  plot$shrinkRate = 0.4
  #plot$plot(samples)
  plot$dynamicPlot(samples)
  #plot$plotToPdf(samples, "direct")
}

# log
if (T) {
  plot = logDotplot()
  plot$logBase = 2
  #plot$plot(samples)
  plot$dynamicPlot(samples)
  #plot$plotToPdf(samples, "direct")
}

