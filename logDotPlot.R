# get source file for base dot plot
if (!exists("sweepDotplot"))
  source("sweepDotplot.R")

# class for logarithmic dotplots
logDotplot = setRefClass(
  Class = "logDotplot",
  contains = "sweepDotplot",
  fields = list(
    logBase = "numeric" # should never be less than (sqrt(5)+1)/2 (golden ratio)
  ),
  methods = list(
    initialize = function() {
      callSuper()
      .self$logBase <- 2
    },

    # calculates the diameter of a single dot in a column with the specified number of dots
    getDotDiameter = function(c) {
      return(startDiameter * logb(c + logBase - 1, logBase) / c)
    },

    dynamicPlot = function(data) {
      if (isAvailable()) {
        updatePlot = function(sd, uar, tar, lb){
          .self$startDiameter <- sd
          .self$useAspectRatio <- uar
          .self$targetAspectRatio <- tar
          .self$logBase <- lb
          .self$plot(data)
        }
        manipulate(
          updatePlot(sd, uar, tar, lb),
          sd=slider(0,5, step=0.1, initial=.self$startDiameter, label = "Start diameter"),
          uar=checkbox(.self$useAspectRatio, label = "Adjust for aspect ratio"),
          tar=slider(0.1,4, step=0.1, initial=.self$targetAspectRatio, label = "Aspect ratio"),
          lb=slider(2, 10, step=1, initial=.self$logBase, label = "Logarithm base")
        )
      }
    }
  )
)
