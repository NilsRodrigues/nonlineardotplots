# get source file for base dot plot
if (!exists("sweepDotplot"))
  source("sweepDotplot.R")

# class for root dotplots
rootDotplot = setRefClass(
  Class = "rootDotplot",
  contains = "sweepDotplot",
  fields = list(
    shrinkRate = "numeric"
  ),
  methods = list(
    initialize = function() {
      callSuper()
      .self$shrinkRate <- 0.4
    },

    # calculates the diameter of a single dot in a column with the specified number of dots
    getDotDiameter = function(c) {
      return(startDiameter * (1/c^shrinkRate))
    },

    dynamicPlot = function(data) {
      if (isAvailable()) {
        updatePlot = function(sd, uar, tar, sr){
          .self$startDiameter <- sd
          .self$useAspectRatio <- uar
          .self$targetAspectRatio <- tar
          .self$shrinkRate <- sr
          .self$plot(data)
        }
        manipulate(
          updatePlot(sd, uar, tar, sr),
          sd=slider(0,5, step=0.1, initial=.self$startDiameter, label = "Start diameter"),
          uar=checkbox(.self$useAspectRatio, label = "Adjust for aspect ratio"),
          tar=slider(0.1,4, step=0.1, initial=.self$targetAspectRatio, label = "Aspect ratio"),
          sr=slider(0,1, step=0.025, initial=.self$shrinkRate, label = "Shrink rate")
        )
      }
    }
  )
)
