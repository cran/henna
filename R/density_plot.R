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
#' @inheritParams riverPlot
#' @param df A data frame with at least two columns, representing the \code{x}
#' and \code{y} coordinates of the points. A score column can also be provided
#' as the third column. Nearest neighbor information can be provided in the last
#' column as a character vector with elements selected from the rownames.
#' @inheritParams riverPlot
#' @param colorScheme Color scheme.
#' @param useSchemeDefaults Whether to use the default \code{segColor},
#' \code{pointColor} and \code{labelColor} for scheme. Ignored
#' if \code{colorScheme} is set to 'custom'.
#' @param drawNN Whether to draw segments linking each point to its nearest
#' neighbor.
#' @param drawScores Whether to render scores on the plot. If set to
#' \code{TRUE}, the third column of the input data frame will be numeric and
#' scores will be taken from there.
#' @param palette Color palette. Used only if color scheme is set to 'custom'.
#' @param segColor Nearest neighbor segment color. Ignored if \code{drawNN} is
#' set to \code{FALSE}, or if \code{useSchemeDefaults} is \code{TRUE} and
#' \code{colorScheme} is different from 'custom'.
#' @param pointSize Point size.
#' @param pointColor Point color. Ignored if \code{useSchemeDefaults}
#' is \code{TRUE} and \code{colorScheme} is different from 'custom'.
#' @param segType Nearest neighbor segment type. Must choose between 'solid',
#' 'dashed', 'dotted','dotdash', 'longdash' and 'twodash'. Ignored if
#' \code{drawNN} is set to \code{FALSE}.
#' @param segWidth Nearest neighbor segment width. Ignored if \code{drawNN} is
#' set to \code{FALSE}.
#' @param legendPos Legend position. Choose between 'right' and 'none'.
#' @param nGridPoints Number of grid points in each direction.
#' @param expandPerc Percentage by which the grid will be expanded.
#' @inheritParams labelPoints
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
                        title = 'Density plot',
                        colorScheme = c('oasis', 'sea', 'lava', 'custom'),
                        useSchemeDefaults = FALSE,
                        drawNN = TRUE,
                        drawScores = FALSE,
                        palette = NULL,
                        segColor = 'plum1',
                        pointSize = 0.8,
                        pointColor = 'red',
                        segType = c('dashed','solid', 'dotted',
                                    'dotdash', 'longdash', 'twodash'),
                        segWidth = 0.4,
                        legendPos = c('right', 'none'),
                        nGridPoints = 300,
                        expandPerc = 20,
                        labelSize = 2.5,
                        labelColor = 'black',
                        labelRepulsion = 1,
                        labelPull = 1,
                        maxOverlaps = Inf,
                        verbose = FALSE,
                        ...){

    colorScheme <- match.arg(colorScheme, c('oasis', 'sea', 'lava', 'custom'))
    segType <- match.arg(segType, c('dashed','solid', 'dotted',
                                    'dotdash', 'longdash', 'twodash'))
    legendPos <- match.arg(legendPos, c('right', 'none'))

    if (colorScheme == 'oasis'){
        palette <- dpColors('oasis')
        if (useSchemeDefaults){
            segColor <- 'plum1'
            pointColor <- 'red'
            labelColor <- 'black'
        }
    }

    if (colorScheme == 'sea'){
        palette <- dpColors('sea')
        if (useSchemeDefaults){
            segColor <- 'thistle'
            pointColor <- 'red'
            labelColor <- 'black'
        }
    }

    if (colorScheme == 'lava'){
        palette <- dpColors('lava')
        if (useSchemeDefaults){
            segColor <- 'dodgerblue3'
            pointColor <- 'black'
            labelColor <- 'black'
        }
    }

    if (colorScheme == 'custom'){
        if (is.null(palette))
            stop('The custom color scheme requires an input palette.')
        palette <- palette
    }

    pointLabs <- rownames(df)
    if(ncol(df) > 2)
        if(drawScores)
            if(is(df[, 3])[1] == 'numeric')
                pointLabs <- mapply(function(x, y) paste0(x, '\n', y),
                                    pointLabs, round(df[, 3], 2)) else
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
        labs(x='x', y='y', fill='Density') +
        theme(legend.position=legendPos)

    if(drawNN){
        lastCol <- df[, ncol(df)]
        if(is(lastCol)[1] != 'character'){
            if(verbose)
                message('Nearest neighbor information not provided.',
                        ' Will be computed.')
        }else{
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

    p <- p + geom_point(size=pointSize, color=pointColor) +
        geom_text_repel(aes(label=pointLabs),
                        size=labelSize,
                        color=labelColor,
                        force=labelRepulsion,
                        force_pull=labelPull,
                        max.overlaps=maxOverlaps)

    p <- centerTitle(p, title, ...)
    return(p)
}
