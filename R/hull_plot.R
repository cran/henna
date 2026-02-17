#' Find the coordinates where a vertical or horizontal line intersects the hull
#'
#' This function finds the coordinates where a vertical or horizontal line
#' intersects the hull.
#'
#'
#' @param df A four-column data frame representing segments.
#' @param axis An integer representing the axis intersected by the vertical or
#' horizontal line, x (1) or y (2).
#' @param axisIntersect The coordinate where the vertical or horizontal line
#' intersects the relevant axis.
#'
#' @return A vector of size two representing the coordinates of the two
#' intersection points between the vertical or horizontal line and the convex
#' hull on the axis different from the input axis.
#'
#' @keywords internal
#'
borderCoords <- function(df, axis, axisIntersect){

    if(is.null(axisIntersect))
        return(c('None', 'None'))

    otherAxis <- axis %% 2 + 1
    axisEnd <- axis + 2
    otherAxisEnd <- otherAxis + 2
    axisVals <- df[, axis]
    axisEndVals <- df[, axisEnd]

    coords <- c(df[axisVals == axisIntersect, ][, otherAxis])

    if (length(coords) < 2){
        df <- df[axisVals < axisIntersect & axisEndVals > axisIntersect |
                     axisVals > axisIntersect & axisEndVals < axisIntersect, ]

        axisVals <- df[, axis]
        axisEndVals <- df[, axisEnd]
        otherAxisVals <- df[, otherAxis]
        otherAxisEndVals <- df[, otherAxisEnd]

        df$diffRatio <- (otherAxisEndVals - otherAxisVals) /
            (axisEndVals  - axisVals)
        df$newCoord <- df$diffRatio * (axisIntersect - axisVals) +
            otherAxisVals

        coords <- c(coords, df$newCoord)
    }

    return(sort(coords))
}

#' Split the convex hull in two along an input line
#'
#' This function splits the convex hull in two along an input vertical or
#' horizontal line.
#'
#'
#' @inheritParams documentFun
#' @param p A ggplot object representing the hull.
#' @inheritParams convexHull
#' @inheritParams borderCoords
#' @param pointCoords The coordinates of the input points on the axis
#' perpendicular to the input border line.
#' @param borderPoints The points where the border line intersects the
#' convex hull.
#' @param alpha Opaqueness level.
#'
#' @return A ggplot object showing the hull split in two parts along the input
#' line.
#'
#' @keywords internal
#'
splitInTwo <- function(p,
                       pointsDF,
                       axisIntersect,
                       pointCoords,
                       borderPoints,
                       legendLabs = paste0('Group ', seq(2)),
                       alpha = 0.5){
    df1 <- rbind(pointsDF[pointCoords < axisIntersect, ], borderPoints)
    df2 <- rbind(pointsDF[pointCoords > axisIntersect, ], borderPoints)

    hullSegments1 <- pointsToSegments(convexHull(df1))
    hullSegments2 <- pointsToSegments(convexHull(df2))

    p <- p + geom_polygon(data=hullSegments1, aes(x=.data[['x']],
                                                  y=.data[['y']],
                                                  fill=legendLabs[1]),
                          alpha=alpha)
    p <- p + geom_polygon(data=hullSegments2, aes(x=.data[['x']],
                                                  y=.data[['y']],
                                                  fill=legendLabs[2]),
                          alpha=alpha)
    return(p)
}

#' Find the coordinates of the points establishing the four divisions of
#' the hull
#'
#' This function finds the coordinates of the points establishing the
#' four divisions of the hull
#'
#' @inheritParams convexHull
#' @param xInt The coordinate where the vertical line intersects the x axis.
#' @param yInt The coordinate where the horizontal line intersects the y axis.
#' @param vCoords The y coordinates of the two points where the vertical line
#' intersects the convex hull.
#' @param hCoords The x coordinates of the two points where the horizontal line
#' intersects the convex hull.
#'
#' @return A data frame with 2 columns representing the 12 points (not unique)
#' determining the boundaries of the hull divisions.
#'
#' @keywords internal
#'
quadBorders <- function(pointsDF, xInt, yInt, vCoords, hCoords){
    a <- c(xInt, hCoords[1], xInt,
           xInt, hCoords[2], xInt,
           xInt, hCoords[1], xInt,
           xInt, hCoords[2], xInt)
    b <- c(yInt, yInt, vCoords[1],
           yInt, yInt, vCoords[2],
           yInt, yInt, vCoords[2],
           yInt, yInt, vCoords[1])
    df <- data.frame(a, b)
    colnames(df) <- colnames(pointsDF)
    return(df)
}

#' Split the convex hull in four parts along two input lines
#'
#' This function splits the convex hull in two parts along a vertical and a
#' horizontal line.
#'
#'
#' @param p A ggplot object representing the hull.
#' @inheritParams splitInTwo
#' @inheritParams quadBorders
#' @param borderPoints The points where the border lines intersects the
#' convex hull.
#'
#' @return A ggplot object showing the hull split in four parts along the input
#' axes.
#'
#' @noRd
#'
splitInFour <- function(p,
                        pointsDF,
                        xInt,
                        yInt,
                        borderPoints,
                        legendLabs = paste0('Group ', seq(4)),
                        alpha = 0.5){
    df1 <- rbind(pointsDF[pointsDF[, 1] < xInt & pointsDF[, 2] < yInt, ],
                 borderPoints[c(1, 2, 3), ])
    df2 <- rbind(pointsDF[pointsDF[, 1] > xInt & pointsDF[, 2] > yInt, ],
                 borderPoints[c(4, 5, 6), ])
    df3 <- rbind(pointsDF[pointsDF[, 1] < xInt & pointsDF[, 2] > yInt, ],
                 borderPoints[c(7, 8, 9), ])
    df4 <- rbind(pointsDF[pointsDF[, 1] > xInt & pointsDF[, 2] < yInt, ],
                 borderPoints[c(10, 11, 12), ])

    hullSegments1 <- pointsToSegments(convexHull(df1))
    hullSegments2 <- pointsToSegments(convexHull(df2))
    hullSegments3 <- pointsToSegments(convexHull(df3))
    hullSegments4 <- pointsToSegments(convexHull(df4))

    p <- p + geom_polygon(data=hullSegments1, aes(x=.data[['x']],
                                                  y=.data[['y']],
                                                  fill=legendLabs[1]),
                          alpha=alpha)
    p <- p + geom_polygon(data=hullSegments2, aes(x=.data[['x']],
                                                  y=.data[['y']],
                                                  fill=legendLabs[2]),
                          alpha=alpha)
    p <- p + geom_polygon(data=hullSegments3, aes(x=.data[['x']],
                                                  y=.data[['y']],
                                                  fill=legendLabs[3]),
                          alpha=alpha)
    p <- p + geom_polygon(data=hullSegments4, aes(x=.data[['x']],
                                                  y=.data[['y']],
                                                  fill=legendLabs[4]),
                          alpha=alpha)
    return(p)
}


#' Split the convex hull in four parts along two input lines
#'
#' This function splits the convex hull in two parts along a vertical and a
#' horizontal line.
#'
#'
#' @inheritParams splitInTwo
#' @param hullSegments Data frame of segments that define the convex hull.
#' @inheritParams quadBorders
#' @param lineColor The color of the horizontal and vertical dividing lines,
#' if provided. If \code{NULL}, no dividing lines will be drawn, though the
#' hull will still be split along these lines if \code{xInt}
#' and/or \code{yInt}are not \code{NULL}.
#' @param lineWidth The width of the horizontal and vertical dividing lines.
#' Ignored if \code{lineColor} is \code{NULL}.
#' @param lineType The type of the horizontal and vertical dividing lines.
#' Choose between 'dashed','solid', 'dotted', 'dotdash', 'longdash' and
#' 'twodash'. Default is 'dashed'. Ignored if \code{lineColor} is \code{NULL}.
#'
#' @return An object of class \code{gg} showing the hull split along the input
#' axes.
#'
#' @keywords internal
#'
splitHull <- function(p,
                      pointsDF,
                      hullSegments,
                      xInt = NULL,
                      yInt = NULL,
                      lineColor = 'navy',
                      lineWidth = 0.3,
                      lineType = c('dashed','solid', 'dotted',
                                   'dotdash', 'longdash', 'twodash'),
                      legendLabs = paste0('Group ', seq(4)),
                      alpha = 0.2){

    lineType <- match.arg(lineType, c('dashed','solid', 'dotted',
                                      'dotdash', 'longdash', 'twodash'))

    vCoords <- borderCoords(hullSegments, 1, xInt)
    hCoords <- borderCoords(hullSegments, 2, yInt)

    if(!is.null(lineColor)){
        if (!is.null(xInt))
            p <- p + geom_segment(aes(x=xInt,
                                      y=vCoords[1],
                                      xend=xInt,
                                      yend=vCoords[2]),
                                  color=lineColor,
                                  linewidth=lineWidth,
                                  linetype=lineType)
        if (!is.null(yInt))
            p <- p + geom_segment(aes(x=hCoords[1],
                                      y=yInt,
                                      xend=hCoords[2],
                                      yend=yInt),
                                  color=lineColor,
                                  linewidth=lineWidth,
                                  linetype=lineType)
    }

    if(is.null(xInt) & is.null(yInt))
        return(p + geom_polygon(data=hullSegments,
                                aes(x=.data[['x']],
                                    y=.data[['y']],
                                    fill=legendLabs[1]),
                                alpha=alpha))
    if(is.null(xInt))
        return(splitInTwo(p, pointsDF, yInt, pointsDF[, 2],
                          list(hCoords, c(yInt, yInt)),
                          legendLabs))

    if(is.null(yInt))
        return(splitInTwo(p, pointsDF, xInt, pointsDF[, 1],
                          list(c(xInt, xInt), vCoords),
                          legendLabs, alpha))


    return(splitInFour(p, pointsDF, xInt, yInt,
                       quadBorders(pointsDF, xInt, yInt, vCoords, hCoords),
                       legendLabs, alpha))
}

#' Plot the convex hull of a set of points
#'
#' This function plots the convex hull of a set of points. It can also draw
#' a vertical or a horizontal line (or both), dividing the hull into areas of
#' different colors.
#'
#' @inheritParams documentFun
#' @inheritParams convexHull
#' @inheritParams splitHull
#' @inheritParams createLabelDFTemplate
#' @param hullWidth Width of the convex hull. If 0 (as default), the convex
#' hull will not be displayed.
#' @param labelSize Label size. Ignored if \code{labelDF} is \code{NULL}.
#' @param labelColor Label color. Ignored if \code{labelDF} is \code{NULL}.
#' @param maxOverlaps Maximum overlaps. Ignored if \code{labelDF}
#' is \code{NULL}.
#'
#' @return An object of class \code{gg}.
#'
#' @examples
#' pointsDF <- data.frame(x = c(1, 2, 4, 7, 10,
#' 12, 13, 15, 16),
#' y = c(1, 1, 2, 3, 3, 2,
#' 1, 2, 1))
#' hullPlot(pointsDF, 'Hull plot', 7, 1.5)
#'
#' @export
#'
hullPlot <- function(pointsDF,
                     title = NULL,
                     xInt = NULL,
                     yInt = NULL,
                     palette = hpColors(),
                     lineColor = 'navy',
                     lineWidth = 0.3,
                     lineType = c('dashed','solid', 'dotted',
                                  'dotdash', 'longdash', 'twodash'),
                     hullWidth = 0,
                     xLab = NULL,
                     yLab = NULL,
                     legendLabs = paste0('Group ', seq(4)),
                     pointSize = 1,
                     pointShape = 4,
                     alpha = 0.2,
                     labeledPoints = NULL,
                     labelOutside = FALSE,
                     labXThr = NULL,
                     labYThr = NULL,
                     labelType = c('free', 'boxed'),
                     labelSize = 2.5,
                     labelColor = 'black',
                     labelRepulsion = 1,
                     labelPull = 0,
                     maxOverlaps = 10,
                     legendPos = 'bottom',
                     legendTextSize = 10,
                     axisTextSize = 12,
                     axisTitleSize = 12,
                     ...){

    labelType <- match.arg(labelType, c('free', 'boxed'))

    if (nrow(pointsDF) < 2)
        stop('The hull plot requires at least two points.')

    pointsX <- pointsDF[, 1]
    pointsY <- pointsDF[, 2]

    if (!is.null(xInt))
        if (xInt <= min(pointsX) | xInt >= max(pointsX))
            stop('xInt is outside the (min(X), max(X)) interval.')

    if (!is.null(yInt))
        if (yInt <= min(pointsY) | yInt >= max(pointsY))
            stop('yInt is outside the (min(Y), max(Y)) interval.')

    if(nrow(pointsDF) > 2)
        pointsDF <- pointsDF[, c(1, 2)]

    hullIndices <- chull(pointsX, pointsY)
    hull <- convexHull(pointsDF, hullIndices)
    hullSegments <- pointsToSegments(hull)

    if (!is.null(xInt) & !is.null(yInt)){
        if(isPointOnBoundary(xInt, yInt, hullSegments))
            stop('The (xInt, yInt) point is on the boundary of the polygon',
                 ' determined by the input points. It must be inside.')

        tempPointsDF <- rbind(pointsDF, c(xInt, yInt))
        tempHullIndices <- chull(tempPointsDF)
        if (!identical(sort(hullIndices),
                       sort(tempHullIndices)))
            stop('The (xInt, yInt) point is outside the polygon',
                 ' determined by the input points. It must be inside.')

        slope <- (hullSegments$yEnd - hullSegments$y) /
            (hullSegments$xEnd - hullSegments$x)
        minDiff <- min(abs((yInt - hullSegments$y) /
                    (xInt - hullSegments$x) - slope))
        if(!minDiff)
            stop('The (xInt, yInt) point is on the boundary of the polygon',
                 ' determined by the input points. It must be inside.')
    }

    p <- ggplot() + theme_classic() +
        labs(x=xLab, y=yLab) +
        theme(legend.position=legendPos,
              legend.text=element_text(size=legendTextSize),
              legend.title=element_blank(),
              axis.text=element_text(size=axisTextSize),
              axis.title=element_text(size=axisTitleSize))

    if(hullWidth)
        p <- p + geom_segment(data=hullSegments,
                              aes(x=.data[['x']],
                                  y=.data[['y']],
                                  xend=.data[['xEnd']],
                                  yend=.data[['yEnd']]),
                              linewidth=hullWidth)

    p <- splitHull(p, pointsDF, hullSegments, xInt, yInt,
                   lineColor, lineWidth, lineType,
                   legendLabs, alpha)
    p <- p + scale_fill_manual(values=palette, labels=legendLabs)

    p <- p + geom_point(data=pointsDF, aes(x=.data[[names(pointsDF)[1]]],
                                           y=.data[[names(pointsDF)[2]]]),
                   size=pointSize, shape=pointShape)

    labelDF <- createLabelDFHull(pointsDF, 1, 2, labeledPoints,
                                 labelOutside, labXThr, labYThr)

    if(!is.null(labelDF))
        p <- labelPoints(p, labelDF, rownames(labelDF), labelType, labelSize,
                         labelColor, labelRepulsion, labelPull, maxOverlaps)

    p <- centerTitle(p, title, ...)
    return(p)
}
