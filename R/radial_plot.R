#' @importFrom dplyr count
#' @importFrom ggeasy easy_remove_axes
#' @importFrom ggforce geom_circle
#' @importFrom ggnewscale new_scale_color new_scale_fill
#' @importFrom ggrepel geom_text_repel
#' @importFrom viridis scale_color_viridis scale_fill_viridis
#' @importFrom withr with_seed
#'
NULL

#' Map values to distances from the center and find the frequency of these
#' distances
#'
#' This function interprets values as distances from a center (high
#' values = low distances) and calculates the frequencies of these distances.
#'
#' @details Used later to draw concentric circles with the frequencies
#' representing the number of points on a circle of the same radius.
#'
#' @param valuesDF A data frame with names on the first column and
#' positive integers on the second column.
#'
#' @return A data frame of distance frequencies.
#'
#' @keywords internal
#'
distFreq <- function(valuesDF){
    df <- dplyr::count(valuesDF, valuesDF[, 2])
    if (nrow(df) == 1)
        center <- df[1, 1] else{
            df <- df[order(df[, 1], decreasing=TRUE), ]
            if (df[2, 1] != df[1, 1])
                center <- df[1, 1] else
                    center <- df[1, 1]
        }
    colnames(df) <- c('Dist', 'Freq')
    df$Dist <- center - df$Dist
    return(df)
}

#' Generate the coordinates of points on a circle centered at origin
#'
#' This function generates nPoints on a circle of radius r
#' centered at origin.
#'
#' @param r Radius.
#' @param nPoints Number of points.
#' @param seed Random seed.
#'
#' @return A data frame with the coordinates of the points.
#'
#' @keywords internal
#'
pointsOnCircle <- function(r, nPoints, seed = 50){
    angleOffset <- with_seed(seed, runif(n=1, min=0, max=2 * pi))
    theta <- 2 * pi / nPoints
    points <- lapply(seq(nPoints),
                     function(k) c(r * cos(k * theta + angleOffset),
                                   r * sin(k * theta + angleOffset)))
    res <- do.call(rbind, points)
    colnames(res) <- c('x', 'y')
    return(res)
}

#' Compute the coordinates of items plotted on the figure made from concentric
#' circles
#'
#' This function computes the coordinates of items on the figure made from
#' concentric circles.
#'
#' @details A wrapper around \code{distFreq} and \code{pointsOnCircle}.
#'
#' @inheritParams distFreq
#' @inheritParams pointsOnCircle
#'
#' @return A data frame containing the coordinates of the items.
#'
#' @noRd
#'
itemCoords <- function(valuesDF, seed = 50){
    valuesDF <- valuesDF[order(valuesDF[, 2], decreasing=TRUE), ]
    distFreqDF <- distFreq(valuesDF)
    circlePoints <- do.call(rbind, lapply(seq_len(nrow(distFreqDF)), function(i)
        pointsOnCircle(distFreqDF$Dist[i], distFreqDF$Freq[i], seed + i)))
    df <- cbind(valuesDF[, 1, drop=FALSE],
                circlePoints,
                valuesDF[, c(2, 3)])
    df[, 5] <- as.factor(df[, 5])
    return(df)
}

#' Store the radii of the circles and the corresponding values
#'
#' This function stores the radii of the circles and the corresponding value
#' for each circle.
#'
#' @param itemCoordsDF Data frame wih item coordinates.
#' @param extraCircles Number of circles drawn beyond those required to include
#' the points.
#'
#' @return A data frame containing the radius and the corresponding value for
#' each circle.
#'
#' @keywords internal
#'
circleCoords <- function(itemCoordsDF, extraCircles = 0){
    values <- itemCoordsDF[, 4]
    minValue <- values[length(values)] - extraCircles
    maxValue <- values[1]
    nCircles <- maxValue - minValue + 1
    hasSharedMax <- 0
    if(length(values) > 1)
        hasSharedMax <- values[1] == values[2]
    df <- data.frame(
        x = rep(0, nCircles),
        y = rep(0, nCircles),
        r = seq(nCircles + hasSharedMax - 0.5, hasSharedMax + 0.5, -1),
        Value = seq(minValue, maxValue))
    return(df)
}

#' Draw radial plot for a data frame with positive integer-valued points
#'
#' This function draws a radial plot for a data frame, plotting positive
#' integer-valued points over concentric circles, with points located
#' more centrally representing higher values.
#'
#' @inheritParams distFreq
#' @inheritParams riverPlot
#' @inheritParams hullPlot
#' @param valueLegendTitle Legend title corresponding to the positive integer
#' column.
#' @param groupLegendTitle Legend title corresponding to the categorical
#' column.
#' @inheritParams circleCoords
#' @param palette Color palette.
#' @param labelSize Label size.
#' @param pointSize Point size.
#' @param legendTitleSize Legend title size.
#' @param legendTextSize Legend text size.
#' @inheritParams pointsOnCircle
#' @param breakDensity Factor used in calculating the number of breaks for
#' the values legend. Higher values of this argument add more breaks to the
#' legend, but no breaks at a distance below 1 will be allowed.
#'
#' @return An object of class \code{gg}.
#'
#' @examples
#' valuesDF <- data.frame(Protein = paste0('P', seq(20)),
#' Value = sample(10, 20, replace=TRUE),
#' Group = sample(3, 20, replace=TRUE))
#' radialPlot(valuesDF)
#'
#' @export
#'
radialPlot <- function(valuesDF,
                       title = 'Radial plot',
                       valueLegendTitle = 'Value',
                       groupLegendTitle = 'Group',
                       extraCircles = 0,
                       palette = rpColors(length(unique(valuesDF[, 3]))),
                       labelSize = 3,
                       pointSize = 0.8,
                       legendTitleSize = 10,
                       legendTextSize = 10,
                       labelRepulsion = 1,
                       labelPull = 0,
                       maxOverlaps = 15,
                       breakDensity = 6,
                       seed = 50,
                       ...){

    itemCoordsDF <- itemCoords(valuesDF, seed)
    circleCoordsDF <- circleCoords(itemCoordsDF, extraCircles)
    legendStep <- as.integer(itemCoordsDF[1, 4] / breakDensity) + 1
    p <- ggplot() +
        geom_circle(aes(x0=.data[['x']],
                        y0=.data[['y']],
                        r=.data[['r']],
                        fill=.data[['Value']],
                        color=.data[['Value']]),
                    data=circleCoordsDF) +
        scale_fill_viridis(option='viridis', begin=0.4,
                           breaks=seq(itemCoordsDF[1, 4], 1, -legendStep)) +
        scale_color_viridis(option='viridis', begin=0.4,
                            breaks=seq(itemCoordsDF[1, 4], 1, -legendStep),
                            guide='none') +
        labs(fill=valueLegendTitle) +
        theme_classic() + easy_remove_axes() + coord_fixed() +
        theme(plot.margin=margin(0, 0, 0, 0),
              legend.title=element_text(size=legendTitleSize),
              legend.text=element_text(size=legendTextSize)) +
        geom_text_repel(aes(x=.data[['x']],
                            y=.data[['y']],
                            label=.data[[names(itemCoordsDF)[1]]]),
                        data=itemCoordsDF, size=labelSize,
                        force=labelRepulsion,
                        force_pull=labelPull,
                        max.overlaps=maxOverlaps)
    if (!is.null(groupLegendTitle))
        p <- p + new_scale_color() +
        new_scale_fill() +
        geom_point(aes(x=.data[['x']],
                       y=.data[['y']],
                       color=.data[[names(itemCoordsDF)[5]]]),
                   data=itemCoordsDF,
                   size=pointSize) +
        scale_color_discrete(type=palette) +
        labs(color=groupLegendTitle) else
            p <- p + geom_point(aes(x=.data[['x']],
                                    y=.data[['y']]),
                                data=itemCoordsDF,
                                color=palette[1],
                                size=pointSize)
    p <- centerTitle(p, title, ...)
    return(p)
}
