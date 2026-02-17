#' @importFrom methods is
#'
NULL

#' Create segments between points and their nearest neighbors.
#'
#' This function creates segments between points and their nearest neighbors.
#'
#' @param df A data frame with two columns representing the \code{x}
#' and \code{y} coordinates of the points.
#'
#' @return A data frame with four columns representing the segments between
#' each point and its nearest neighbor.
#'
#' @noRd
#'
createPairSegments <- function(df){
    df <- addNNCol(df)
    segmentsDF <- cbind(df[, c(1, 2)], df[df$nn, c(1, 2)])
    colnames(segmentsDF) <- c('x', 'y', 'xend', 'yend')
    return(segmentsDF)
}

#' Create density plot
#'
#' This function creates a density plot.
#'
#' @inheritParams documentFun
#' @inheritParams labelPoints
#' @param df A data frame with at least two columns, representing the \code{x}
#' and \code{y} coordinates of the points. A score column can also be provided
#' as the third column. Nearest neighbor information can be provided in the last
#' column as a character vector with elements selected from the rownames.
#' @param colorScheme Color scheme. Choose between 'cake', 'cloudy', 'grapes',
#' 'lava', oasis', 'orichalc', 'sea', 'sky' and 'custom'. Default is 'cloudy'.
#' @param useSchemeDefaults Whether to use the default \code{segColor},
#' \code{pointColor} and \code{labelColor} values for scheme. Ignored
#' if \code{colorScheme} is set to 'custom'.
#' @param drawNN Whether to draw segments linking each point to its nearest
#' neighbor.
#' @param drawScores Whether to render scores on the plot. If set to
#' \code{TRUE}, the third column of the input data frame will be numeric and
#' scores will be taken from there.
#' @param palette A character vector of colors. Used only if color scheme is
#' set to 'custom'.
#' @param segColor Nearest neighbor segment color. Ignored if \code{drawNN} is
#' set to \code{FALSE}, or if \code{useSchemeDefaults} is \code{TRUE} and
#' \code{colorScheme} is different from 'custom'.
#' @param pointSize Point size.
#' @param pointColor Point color. Ignored if \code{useSchemeDefaults}
#' is \code{TRUE} and \code{colorScheme} is different from 'custom'.
#' @param segType Nearest neighbor segment type. Choose between 'solid',
#' 'dashed', 'dotted','dotdash', 'longdash' and 'twodash'. Ignored if
#' \code{drawNN} is set to \code{FALSE}.
#' @param segWidth Nearest neighbor segment width. Ignored if \code{drawNN} is
#' set to \code{FALSE}.
#' @param legendPos Legend position. Choose between 'right' and 'none'.
#' @param nGridPoints Number of grid points in each direction.
#' @param expandPerc Percentage by which the grid will be expanded.
#' @param labelColor Label color. Ignored if \code{useSchemeDefaults}
#' is \code{TRUE} and \code{colorScheme} is different from 'custom'.
#' @param verbose Whether output should be verbose.
#'
#' @return An object of class \code{gg}.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 6, 7, 8, 10, 12, 11, 3, 6, 4, 1, 13, 13, 14, 18, 16)
#' y <- c(1, 3, 1, 4, 3, 2, 8, 2, 1, 11, 8, 8, 10, 14, 13, 11, 11, 12,15)
#' z <- round(runif(19, 75, 100), 2)
#' df <- data.frame(x, y, z)
#' rownames(df) <- paste0('p', rownames(df))
#' densityPlot(df)
#'
#' @export
#'
densityPlot <- function(df,
                        title = NULL,
                        colorScheme = c('cloudy', 'cake', 'grapes', 'lava',
                                        'oasis', 'orichalc', 'sea', 'sky',
                                        'custom'),
                        useSchemeDefaults = TRUE,
                        drawNN = TRUE,
                        drawScores = FALSE,
                        xLab = NULL,
                        yLab = NULL,
                        legendTitle = 'Density',
                        palette = NULL,
                        segColor = 'black',
                        pointSize = 1,
                        pointColor = 'red',
                        segType = c('dashed','solid', 'dotted',
                                    'dotdash', 'longdash', 'twodash'),
                        segWidth = 0.4,
                        nGridPoints = 300,
                        expandPerc = 20,
                        labelType = c('free', 'boxed'),
                        labelSize = 3,
                        labelColor = 'black',
                        labelRepulsion = 1,
                        labelPull = 1,
                        maxOverlaps = 10,
                        labelPadding = 0,
                        boxPadding = 0,
                        labelSegWidth = 0.4,
                        legendPos = c('right', 'none'),
                        legendTextSize = 10,
                        legendTitleSize = 10,
                        axisTextSize = 12,
                        axisTitleSize = 12,
                        verbose = FALSE,
                        ...){

    colorScheme <- match.arg(colorScheme, c('cloudy', 'cake', 'grapes',
                                            'lava', 'oasis', 'orichalc',
                                            'sea', 'sky', 'custom'))
    segType <- match.arg(segType, c('dashed','solid', 'dotted',
                                    'dotdash', 'longdash', 'twodash'))
    labelType <- match.arg(labelType, c('free', 'boxed'))
    legendPos <- match.arg(legendPos, c('right', 'none'))

    if (colorScheme == 'custom'){
        if (is.null(palette))
            stop('The custom color scheme requires an input palette.')
        palette <- palette
    } else {
        palette <- dpColors(colorScheme)
        if (useSchemeDefaults){
            colorsDF <- lpsColors()
            labelColor <- colorsDF['label', colorScheme]
            pointColor <- colorsDF['point', colorScheme]
            segColor <- colorsDF['segment', colorScheme]
        }
    }

    pointLabs <- rownames(df)
    if(ncol(df) > 2)
        if(drawScores)
            if(is(df[, 3])[1] == 'numeric')
                pointLabs <- mapply(function(x, y) paste0(x, '\n', y),
                                       rownames(df), round(df[, 3], 2)) else
                                        warning('drawScores is set to TRUE but ',
                                                'no scores are available in ',
                                                'the third column. Scores will ',
                                                'not be plotted.')

    p <- ggplot(df, aes(x=.data[[names(df)[1]]], y=.data[[names(df)[2]]])) +
        stat_density_2d(aes(fill=after_stat(density)),
                        geom="raster",
                        contour=FALSE,
                        n=nGridPoints) +
        scale_fill_gradientn(colors=palette) + theme_classic() +
        expand_limits(x=expandRange(df[, 1], expandPerc),
                      y=expandRange(df[, 2], expandPerc)) +
        labs(x=xLab, y=yLab, fill=legendTitle) +
        theme(legend.position=legendPos,
              legend.text=element_text(size=legendTextSize),
              legend.title=element_text(size=legendTitleSize),
              axis.text=element_text(size=axisTextSize),
              axis.title=element_text(size=axisTitleSize))

    if(drawNN){
        lastCol <- df[, ncol(df)]
        if(is(lastCol)[1] != 'character'){
            if(verbose)
                message('Nearest neighbor information not provided.',
                        ' Will be computed.')
        } else{
            extraNames <- setdiff(lastCol, rownames(df))
            if (length(extraNames))
                warning(extraNames[1], ' found in the last column but not',
                        ' in row names. Nearest neighbor information will be',
                        ' computed to replace the last column.')
        }
        segmentsDF <- createPairSegments(df)
        p <- p + geom_segment(data=segmentsDF,
                              aes(x=.data[['x']],
                                  y=.data[['y']],
                                  xend=.data[['xend']],
                                  yend=.data[['yend']]),
                              color=segColor,
                              linetype=segType,
                              linewidth=segWidth)
    }

    p <- p + geom_point(size=pointSize, color=pointColor)
    p <- labelPoints(p, df, pointLabs, labelType, labelSize, labelColor,
                     labelRepulsion, labelPull, maxOverlaps,
                     labelPadding, boxPadding, labelSegWidth)

    p <- centerTitle(p, title, ...)
    return(p)
}
