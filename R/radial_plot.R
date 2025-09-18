#'@importFrom dplyr count
#'@importFrom ggeasy easy_remove_axes
#'@importFrom ggforce geom_circle
#'@importFrom ggnewscale new_scale_color new_scale_fill
#'@importFrom ggrepel geom_text_repel
#'@importFrom viridis scale_color_viridis scale_fill_viridis
#'
NULL

#' Map degrees to distances from the center and find the frequency of these
#' distances
#'
#' This function interprets degrees as distances from a center (high
#' degrees = low distances) and calculates the frequencies of these distances.
#' Used later to draw concentric circles with the frequencies representing the
#' number of points on a circle of the same radius.
#'
#' @param degreesDF A data frame with names on the first column and
#' positive integers on the second column.
#'
#' @return A data frame of distance frequencies.
#'
#' @keywords internal
#'
distFreq <- function(degreesDF){
    message('Finding degree frequencies...')
    df <- dplyr::count(degreesDF, degreesDF[, 2])
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
#'
#' @return A data frame with the coordinates of the points.
#'
#' @noRd
#'
pointsOnCircle <- function(r, nPoints){
    angleOffset <- runif(n=1, min=0, max=2 * pi)
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
#'
#' @return A data frame containing the coordinates of the items.
#'
#' @noRd
#'
itemCoords <- function(degreesDF){
    degreesDF <- degreesDF[order(degreesDF[, 2], decreasing=TRUE), ]
    distFreqDF <- distFreq(degreesDF)
    message('Finding coordinates...')
    circlePoints <- do.call(rbind, lapply(seq_len(nrow(distFreqDF)), function(i)
        pointsOnCircle(distFreqDF$Dist[i], distFreqDF$Freq[i])))
    df <- cbind(degreesDF[, 1, drop=FALSE], circlePoints, degreesDF[, c(2, 3)])
    df[, 5] <- as.factor(df[, 5])
    return(df)
}

#' Store the radii of the circles and the corresponding number of edges
#'
#' This function stores the radii of the circles and the corresponding number
#' of edges.
#'
#' @param itemCoordsDF Dataframe wih item coordinates
#' @param extraCircles Number of circles drawn beyond those required to include
#' the points representing the genes.
#'
#' @return A data frame containing the radius and the number of edges for each
#' circle.
#'
#' @keywords internal
#'
circleCoords <- function(itemCoordsDF, extraCircles = 0){
    degrees <- itemCoordsDF[, 4]
    minDegree <- degrees[length(degrees)] - extraCircles
    maxDegree <- degrees[1]
    nCircles <- maxDegree - minDegree + 1
    hasSharedMax <- 0
    if(length(degrees) > 1)
        hasSharedMax <- degrees[1] == degrees[2]
    df <- data.frame(
        x = rep(0, nCircles),
        y = rep(0, nCircles),
        r = seq(nCircles + hasSharedMax - 0.5, hasSharedMax + 0.5, -1),
        Degree = seq(minDegree, maxDegree))
    return(df)
}

#' Draw radial plot for a degrees data frame
#'
#' This function draws a radial plot for a degrees data frame, plotting
#' positive integer-valued points over concentric circles, with points located
#' more centrally representing higher values.
#'
#' @inheritParams distFreq
#' @inheritParams riverPlot
#' @inheritParams hullPlot
#' @param degreeLegendTitle Legend title corresponding to the positive integer
#' column.
#' @param groupLegendTitle Legend title corresponding to the categorical
#' column.
#' @inheritParams circleCoords
#' @param palette Color palette.
#' @param labelSize Label size.
#' @param pointSize Point size.
#'
#' @return An object of class \code{gg}.
#'
#' @examples
#' degreesDF <- data.frame(Protein = paste0('P', seq(20)),
#' Degree = sample(10, 20, replace=TRUE),
#' Group = sample(3, 20, replace=TRUE))
#' radialPlot(degreesDF)
#'
#' @export
#'
radialPlot <- function(degreesDF,
                       title = 'Radial plot',
                       degreeLegendTitle = 'Degree',
                       groupLegendTitle = 'Group',
                       extraCircles = 0,
                       palette = rpColors(length(unique(degreesDF[, 3]))),
                       labelSize = 3,
                       pointSize = 0.8,
                       labelRepulsion = 1,
                       labelPull = 0,
                       maxOverlaps = 10,
                       ...){

    itemCoordsDF <- itemCoords(degreesDF)
    circleCoordsDF <- circleCoords(itemCoordsDF, extraCircles)
    legendStep <- as.integer(itemCoordsDF[1, 4] / 6) + 1
    p <- ggplot() +
        geom_circle(aes(x0=.data[['x']],
                        y0=.data[['y']],
                        r=.data[['r']],
                        fill=.data[['Degree']],
                        color=.data[['Degree']]),
                    data=circleCoordsDF) +
        scale_fill_viridis(option='viridis', begin=0.4,
                           breaks=seq(itemCoordsDF[1, 4], 1, -legendStep)) +
        scale_color_viridis(option='viridis', begin=0.4,
                            breaks=seq(itemCoordsDF[1, 4], 1, -legendStep),
                            guide='none') +
        labs(fill=degreeLegendTitle) +
        theme_classic() + easy_remove_axes() + coord_fixed() +
        theme(plot.margin=margin(0, 0, 0, 0),
              legend.title=element_text(size=10),
              legend.text=element_text(size=10)) +
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
