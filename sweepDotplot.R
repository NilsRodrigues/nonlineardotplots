# get source file for base dot plot
if (!exists("baseDotplot"))
    source("baseDotplot.R")

# imports
library(manipulate)

# base class with parameters
sweepDotplot <- setRefClass(
  Class = "sweepDotplot",
  contains = c("baseDotplot"),

  methods = list(
    initialize = function() {
      callSuper()
      .self$title <- "Sweep Dot Plot"
    },

    # calculates the diameter of a single dot in a column with the specified number of dots
    getDotDiameter = function(c) {
      return(.self$startDiameter)
    },

    upwardsPass = function(data) {
      # create the columns
      columns = .self$initializeColumns()
      diameter = .self$startDiameter
      height = 1
      position = data[1]

      for (i in 2:length(data)) {
        diameter = .self$getDotDiameter(height)
        if (data[i] - position < diameter) {
          height = height + 1
        } else {
          columns <- .self$addColumn(columns, height, position, diameter, data[(i-height):(i-1)])
          #containedValues[[length(containedValues) + 1]] <- ]

          height = 1
          position = data[i]
        }
      }

      # finish the last column
      columns <- .self$addColumn(columns,
        height,
        position,
        .self$getDotDiameter(height),
        data[(length(data) - height):length(data)]
      )

      return(columns)
    },

    downwardsPass = function(data) {
      # create the columns
      columns = .self$initializeColumns()
      diameter = .self$startDiameter
      height = 1
      position = data[length(data)]

      for (i in (length(data)-1):1) {
        diameter = .self$getDotDiameter(height)
        if (position - data[i] < diameter) {
          height = height + 1
        } else {
          columns = .self$addColumn(columns, height, position, diameter, data[(i+1):(i+height)])

          height = 1
          position = data[i]
        }
      }

      # finish the last column
      columns = .self$addColumn(
        columns,
        height,
        position,
        .self$getDotDiameter(height),
        data[(length(data) - height):length(data)]
      )

      return(columns)
    },

    mergePasses = function(upwards, downwards, data) {
      assertthat::are_equal(nrow(upwards$columns), nrow(downwards$columns))
      colCount = nrow(upwards$columns)
      both = upwards
      remainder = 0.0
      mergedValueCount = 0L

      # Position can be simply averaged
      both$columns[,"position"] <- (upwards$columns[,"position"] + rev(downwards$columns[,"position"])) / 2

      remainder = 0.0
      for (i in 1:colCount) {
        up = upwards$columns[i,]
        down = downwards$columns[colCount-i+1,]
        merged = both$columns[i,]

        # Position can be simply averaged
        #merged[[i]]$"position" <- (up$position + down$position) / 2

        # Height has to be an integral number.
        # Could have a remainder of 0.5. Carry it on until the next remainder.
        exactHeight = (up["height"] + down["height"]) / 2 + remainder
        roundedHeight = as.integer(floor(exactHeight))
        merged["height"] <- roundedHeight
        remainder = exactHeight - roundedHeight

        # Calculate current diameter for new height
        merged["diameter"] <- .self$getDotDiameter(roundedHeight)

        # Get the data that is contained in this column
        both$data[[i]] <- data[(mergedValueCount + 1):(mergedValueCount + roundedHeight)]
        merged["dataIndex"] <- i
        mergedValueCount = mergedValueCount + roundedHeight;

        both$columns[i,] = merged
      }

      return(both)
    },

    layout = function(data) {
      up = upwardsPass(data)
      down = downwardsPass(data)
      merged = mergePasses(up, down, data)
      return(merged)
    },


    plot = function(data) {
      # do the layout
      columns = .self$layout(data)

      #measure the plot size
      measure = .self$measurePlot(columns)

      # render the dots
      .self$plotColumns(columns, measure)
    },
    plotToPdf = function(data, filepath) {
      # do the layout
      columns = .self$layout(data)

      #measure the plot size
      measure = .self$measurePlot(columns)

      # prepare the pdf
      if(!endsWith(filepath, ".pdf"))
        filepath = paste(filepath, ".pdf", sep="")
      pdf(filepath, width=measure$deltaX, height=measure$deltaY+1)

      # render the dots
      .self$plotColumns(columns, measure)

      # close the pdf
      dev.off()
    },
    dynamicPlot = function(data) {
      if (isAvailable()) {
        updatePlot = function(sd, uar, tar){
          .self$startDiameter <- sd
          .self$useAspectRatio <- uar
          .self$targetAspectRatio <- tar
          .self$plot(data)
        }
        manipulate(
          updatePlot(sd, uar, tar),
          sd=slider(0,5, step=0.1, initial=.self$startDiameter, label = "Start diameter"),
          uar=checkbox(.self$useAspectRatio, label = "Adjust for aspect ratio"),
          tar=slider(0.1,4, step=0.1, initial=.self$targetAspectRatio, label = "Aspect ratio")
        )
      }
    }
  )
)
