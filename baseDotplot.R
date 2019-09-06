# base class with parameters
baseDotplot <- setRefClass(
  Class = "baseDotplot",

  fields = list(
    title = "character",
    startDiameter = "numeric",
    useAspectRatio = "logical",
    targetAspectRatio = "numeric",
    spacing = "numeric",
    maxYAxisTicks = "numeric"
  ),

  methods = list(
    initialize = function() {
      .self$title <- "Base Dot Plot"
      .self$startDiameter <- 1
      .self$useAspectRatio <- FALSE
      .self$targetAspectRatio <- 16/9
      .self$spacing <- 0.1
      .self$maxYAxisTicks <- 6
    },

    initializeColumns = function() {
      cols = matrix(data=1, nrow=0, ncol=6)
      colnames(cols) = c("height", "position", "diameter", "start", "end", "dataIndex")

      wrapper = list()
      wrapper$columns <- cols
      wrapper$data <- list()
      return (wrapper)
    },
    addColumn = function(columns, height, position, diameter, containedDataValues) {
      index = nrow(columns$columns)+1

      columns$columns <- rbind(
        columns$columns,
        c(
          height,
          position,
          diameter,
          position - diameter/2,
          position + diameter/2,
          index)
        )
      columns$data[[index]] <- containedDataValues

      return(columns)
    },

    plotColumns = function(columns, measure) {
      # initialize the plot
      plot.new()
      title(title, line=3)
      plot.window(xlim=c(measure$minX, measure$maxX), ylim=c(measure$minY, measure$maxY))
      box()
      yAxis(measure$maxHeight)
      xAxis()

      # draw each column
      xRange = measure$maxX - measure$minX
      apply(
        columns$columns,
        1,
        function(column) {
          plotColumn(
            column["position"],
            column["height"],
            column["diameter"]
          )
        })
    },

    plotColumn = function (x, height, diameter) {
      radius = diameter / 2;
      dotSize = radius * (1 - .self$spacing)

      radiusSet = vector(mode="numeric", height)
      radiusSet[] = dotSize
      xSet = vector(mode="numeric", height)
      xSet[] = x
      ySet = (1:height) * diameter - radius

      symbols(xSet, ySet, circles=radiusSet, inches=FALSE, bg="black", fg=NULL, add=TRUE)
      #symbols(x=x, y=ySet, circles=dotSize, inches=FALSE, bg="black", fg=NULL, add=TRUE)
    },

    measurePlot = function(columns) {
      cols = columns$columns

      # get vectors to the column list content
      #positions = unlist(lapply(columns, "[[", "position"))
      #heights = unlist(lapply(columns, "[[", "height"))
      #diameters = unlist(lapply(columns, "[[", "diameter"))

      # find the margins of the plot

      minX = min(cols[,"position"]) - max(cols[,"diameter"]) / 2
      maxX = max(cols[,"position"]) + max(cols[,"diameter"]) / 2
      deltaX = maxX - minX

      maxHeight = max(cols[,"height"])
      minY = 0
      maxY = max(cols[,"height"] * cols[,"diameter"])
      deltaY = maxY - minY

      return(list(
        minX = minX, maxX = maxX,
        minY = minY, maxY = maxY,
        deltaX = deltaX, deltaY = deltaY,
        maxHeight = maxHeight))
    },

    xAxis = function() {
      # if (xAxisTickPerPoint) {
      #   tickColor = NULL
      #   if (theObject@storeToPdf) # the alpha channel is not supported in r studio
      #     tickColor = rgb(0,0,0, alpha=0.2)
      #   axis(side=1, at=samples, labels=F, lwd=0, lwd.ticks=1, col.ticks = tickColor) # axis at the bottom with one tick per data point
      #   axis(side=1, tick=T, line=0.5, lwd=0, lwd.ticks=1) # axis beneath the line chart (no base line, only ticks)
      #   axis(side=3, lwd=0, lwd.ticks=1) # axis at the top with only little ticks
      # }
      # else {
      axis(side=1, lwd=0, lwd.ticks=1)
      #axis(side=3, lwd=0, lwd.ticks=1)
      # }
    },
    yAxis = function (maxHeight) {
      tickCount = min(maxHeight, .self$maxYAxisTicks)
      step = (maxHeight-1) / (tickCount-1)

      ticks = vector(mode="numeric", tickCount+1)
      labels = vector(mode="numeric", tickCount+1)

      for (i in 1:tickCount) {
        dotCount = floor(1 + step * (i-1))
        labels[i] = dotCount
        ticks[i] = .self$getDotDiameter(dotCount) * dotCount
      }

      axis(side=2, at=ticks, labels=labels, lwd=0, lwd.ticks=1)
      #axis(side=4, at=ticks, labels=labels, lwd=0, lwd.ticks=1)
    }
  )
)
