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
      .self$maxYAxisTicks <- 5
    },

    createColumn = function(
      height,
      position,
      diameter,
      samples = vector(mode="numeric")) {
      return(list(
        height = height,
        position = position,
        diameter = diameter,
        start = position - (diameter / 2),
        end = position + (diameter / 2),
        samples = samples))
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
      lapply(columns, function(column) { plotColumn(column$position, column$height, column$diameter) })
    },

    plotColumn = function (x, height, diameter) {
      radius = diameter / 2;

      xSet = vector(mode="numeric", height)
      ySet = vector(mode="numeric", height)
      radiusSet = vector(mode="numeric", height)

      for (i in 1:height) {
        xSet[i] = x
        ySet[i] = i * diameter - radius
        radiusSet[i] = radius * (1 - .self$spacing)
      }

      symbols(xSet, ySet, circles=radiusSet, inches=FALSE, bg="black", fg=NULL, add=TRUE)
    },

    measurePlot = function(columns) {
      # get vectors to the column list content
      positions = unlist(lapply(columns, "[[", "position"))
      heights = unlist(lapply(columns, "[[", "height"))
      diameters = unlist(lapply(columns, "[[", "diameter"))

      # find the margins of the plot
      minX = min(positions) - .self$startDiameter / 2
      maxX = max(positions) + .self$startDiameter / 2
      minY = 0
      maxY = max(heights * diameters)
      deltaX = maxX - minX
      deltaY = maxY - minY
      maxHeight = max(heights)

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
      axis(side=3, lwd=0, lwd.ticks=1)
      # }
    },
    yAxis = function (maxHeight) {
      tickCount = min(maxHeight, .self$maxYAxisTicks)
      stepWidth = (maxHeight - 1) / tickCount
      diameter = .self$startDiameter
      radius = .self$startDiameter / 2

      ticks = vector(mode="numeric", tickCount + 1)
      labels = vector(mode="numeric", tickCount + 1)

      ticks[1] = 1 * diameter - radius
      labels[1] = 1
      for (i in 1:tickCount) {
        ticks[i + 1] = floor(i * stepWidth) * diameter - radius
        labels[i + 1] = floor(i * stepWidth)
      }

      axis(side=2, at=ticks, labels=labels, lwd=0, lwd.ticks=1)
      axis(side=4, lwd=0, lwd.ticks=1)
    }
  )
)
