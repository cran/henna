#' Create a label data frame
#'
#' This function creates a label data frame to be used by plotting functions
#' that label selected points.
#'
#' @param df A data frame.
#' @param xCol Column representing the \code{x} axis.
#' @param yCol Column representing the \code{y} axis.
#' @param labeledPoints Point labels to be displayed on the plot. Default is
#' \code{NULL}, entailing that point labels will be displayed on the basis
#' of \code{labXThr} and \code{labYThr}, if at least one of them is not
#' \code{NULL}.
#' @param labelOutside Display labels for points specified by
#' \code{labeledPoints} even if they fall outside the boundaries imposed by
#' \code{labXThr} and \code{labYThr}. Ignored if \code{labeledPoints} is
#' \code{NULL} or both \code{labXThr} and \code{labYThr} are \code{NULL}.
#' @param labXThr Threshold used to plot labels based on \code{xCol} values.
#' @param labYThr Threshold used to plot labels based on \code{yCol} values.
#' @param adjFunX Function used to adjust \code{xCol} values. Default is
#' \code{identity} (no adjustment will be performed).
#' @param adjFunY Function used to adjust \code{yCol} values. Default is
#' \code{identity} (no adjustment will be performed).
#' @param compFunX Function used for the selection of \code{xCol} values
#' based on \code{labXThr}.
#' @param compFunY Function used for the selection of \code{yCol} values
#' based on \code{labYThr}.
#'
#' @return A label data frame.
#'
#' @keywords internal
#'
createLabelDFTemplate <- function(df,
                                  xCol = 1,
                                  yCol = 2,
                                  labeledPoints = NULL,
                                  labelOutside = FALSE,
                                  labXThr = NULL,
                                  labYThr = NULL,
                                  adjFunX = identity,
                                  adjFunY = identity,
                                  compFunX = `>=`,
                                  compFunY = `>=`){
    if (min(vapply(list(labXThr, labYThr, labeledPoints),
                   is.null, integer(1))))
        return(NULL)

    if (min(vapply(c(labXThr, labYThr), is.null, integer(1))))
        return(df[labeledPoints, ])

    xyDF <- df[, c(xCol, yCol)]
    if (!is.null(labXThr))
        xyDF <- xyDF[compFunX(adjFunX(xyDF[, 1]), labXThr), ]
    if (!is.null(labYThr))
        xyDF <- xyDF[compFunY(adjFunY(xyDF[, 2]), labYThr), ]

    if(is.null(labeledPoints) | !labelOutside)
        return(xyDF)

    return(df[union(rownames(xyDF), labeledPoints), ])
}

#' Create a label data frame to be used by \code{hullPlot}.
#'
#' This function creates a label data frame to be used by \code{hullPlot}.
#'
#' @inheritParams createLabelDFTemplate
#'
#' @return A label data frame to be used by \code{hullPlot}.
#'
#' @noRd
#'
createLabelDFHull <- function(df,
                              xCol = 1,
                              yCol = 2,
                              labeledPoints = NULL,
                              labelOutside = FALSE,
                              labXThr = NULL,
                              labYThr = NULL)
    return(createLabelDFTemplate(df, xCol, yCol, labeledPoints, labelOutside,
                                 labXThr, labYThr))

#' Create a label data frame to be used by \code{hullPlot}.
#'
#' This function creates a label data frame to be used by \code{hullPlot}.
#'
#' @inheritParams createLabelDFTemplate
#'
#' @return A label data frame to be used by \code{hullPlot}.
#'
#' @noRd
#'
createLabelDFVolcano <- function(df,
                                 xCol = 2,
                                 yCol = 5,
                                 labeledPoints = NULL,
                                 labelOutside = FALSE,
                                 labXThr = NULL,
                                 labYThr = NULL){

    labelDF <- createLabelDFTemplate(df, xCol, yCol, labeledPoints,
                                     labelOutside, labXThr, labYThr,
                                     adjFunX=abs, compFunY=`<`)

    df[, yCol][df[, yCol] == 0] <- 2
    labelDF[, 2][labelDF[, 2] == 0] <- min(df[, yCol]) / 10

    labelDF[, 2] <- -log(labelDF[, 2], 10)
    return(labelDF)
}

#' Label points in a ggplot object
#'
#' This function labels points in a ggplot object.
#'
#' @inheritParams centerTitle
#' @inheritParams documentFun
#' @param labelDF Label data frame.
#' @param pointLabs Labels of points.
#' @param ... Additional arguments passed to \code{geom_text_repel}
#' (if \code{labelType} is 'free') or \code{geom_text_label}
#' (if \code{labelType} is 'boxed').
#'
#' @return A ggplot object.
#'
#' @examples
#' filePath <- system.file('extdata', 'hullPlot.qs2', package='henna')
#' sharedDF <- qs2::qs_read(filePath)
#' name1 <- 'alpha'
#' name2 <- 'delta'
#' legendLabs <- as.factor(c('Non-top',
#' 'Shared',
#' paste0('Top only for ', name2),
#' paste0('Top only for ', name1)))
#' p <- hullPlot(sharedDF, 'Shared markers plot', xInt=1.5, yInt=1.3,
#' xLab=paste0('avg_log2FC (', name1, ')'),
#' yLab=paste0('avg_log2FC (', name2, ')'),
#' legendLabs=legendLabs)
#' labelDF <- sharedDF[sharedDF[, 'avg_log2FC_1'] > 1.5 &
#' sharedDF[, 'avg_log2FC_2'] > 1.3, ]
#' p <- labelPoints(p, labelDF, labelType='boxed', nudge_x=0.1, nudge_y=0.1)
#'
#' @export
#'
labelPoints <- function(p,
                        labelDF,
                        pointLabs = rownames(labelDF),
                        labelType = c('free', 'boxed'),
                        labelSize = 2.5,
                        labelColor = 'black',
                        labelRepulsion = 1,
                        labelPull = 1,
                        maxOverlaps = 50,
                        boxPadding = 0.2,
                        labelPadding = 0.1,
                        labelSegWidth = 0.4,
                        ...){

    labelType <- match.arg(labelType, c('free', 'boxed'))
    plotFuns <- setNames(list(geom_text_repel, geom_label_repel),
                         c('free', 'boxed'))
    basicArgs <- list(mapping=aes(x=labelDF[, 1],
                                  y=labelDF[, 2],
                                  label=pointLabs),
                      data=labelDF,
                      size=labelSize,
                      color=labelColor,
                      force=labelRepulsion,
                      force_pull=labelPull,
                      max.overlaps=maxOverlaps,
                      box.padding=boxPadding,
                      segment.size=labelSegWidth)
    extraArgs <- list(...)
    if(labelType == 'boxed')
        extraArgs <- c(label.padding=labelPadding,
                       extraArgs)
    allArgs <- c(basicArgs, extraArgs)
    p <- p + do.call(plotFuns[[labelType]], allArgs)
    return(p)
}
