#' Plot a numeric matrix or data frame
#'
#' This function plots a numeric matrix or data frame.
#'
#' @param mat A numeric matrix or data frame.
#' @inheritParams classPlot
#' @param sigDigits Number of significant digits to be displayed for each
#' matrix element.
#' @param isCor Whether the matrix is a correlation matrix, in which case the
#' limits of the color scale will be set to [-1, 1].
#' @param tileBoundaryColor Tile boundary color.
#' @param tileBoundaryWidth Tile boundary width.
#' @inheritParams rankPlot
#' @inheritParams networkPlot
#' @param reverseColors Whether to reverse the order of colors in the palette.
#'
#' @return An object of class \code{gg}.
#'
#' @examples
#' mat <- matrix(round(runif(100, 0, 1), 2), nrow=10)
#' rownames(mat) <- paste0('R', seq(10))
#' colnames(mat) <- paste0('C', seq(10))
#' tilePlot(mat)
#'
#' @export
#'
tilePlot <- function(mat,
                     title = 'Tile plot',
                     xLab = 'x',
                     yLab = 'y',
                     legendLab = 'Value',
                     sigDigits = 2,
                     isCor = FALSE,
                     labelSize = 3,
                     labelColor = 'black',
                     tileBoundaryColor = 'white',
                     tileBoundaryWidth = 0.2,
                     palette = 'Spectral',
                     reverseColors = TRUE,
                     xAngle = 45,
                     vJust = 0.6,
                     ...){
    if (is(mat)[1] != 'matrix')
        mat <- as.matrix(mat)

    mat <- round(mat, sigDigits)
    df <- reshape2::melt(mat)

    palColors <- hcl.colors(50, palette)
    if (reverseColors)
         palColors <- rev(palColors)

    if(isCor){
        limits <- c(-1, 1)
        df <- df[order(df[, 2], decreasing=TRUE), ]
        df <- df[order(df[, 1]), ]
        df[, 2] <- factor(df[, 2], levels=unique(df[, 2]))
        xLab <- NULL
        yLab <- NULL
    }else
        limits <- c(min(df[, 3]), max(df[, 3]))

    p <- ggplot(df, aes(x=.data[[names(df)[1]]],
                        y=.data[[names(df)[2]]],
                        fill=.data[[names(df)[3]]])) +
        geom_tile(color=tileBoundaryColor, lwd=tileBoundaryWidth) +
        theme_classic() +
        theme(axis.text.x=element_text(angle=xAngle, vjust=vJust)) +
        geom_text(aes(label=.data[[names(df)[3]]]),
                  color=labelColor,
                  size=labelSize) +
        scale_fill_gradientn(colors=palColors, limits=limits) +
        labs(x=xLab, y=yLab, fill=legendLab)

    p <- centerTitle(p, title, ...)
    return(p)
}

#' Plot a correlation matrix
#'
#' This function plots a correlation matrix.
#'
#' @details A thin wrapper around \code{tilePlot}.
#'
#' @inheritParams tilePlot
#' @param ... Additional parameters passed to tilePlot.
#'
#' @return An object of class \code{gg}.
#'
#' @examples
#' mat <- matrix(runif(100, -1, 1), nrow=10)
#' colnames(mat) <- paste0('I', seq(10))
#' mat <- round(cor(mat), 2)
#' correlationPlot(mat)
#'
#' @export
#'
correlationPlot <- function(mat, title='Correlation plot',
                            legendLab = 'Correlation', ...)
    return(tilePlot(mat, title, legendLab = legendLab, isCor=TRUE, ...))
