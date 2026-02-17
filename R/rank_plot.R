#' @importFrom liver minmax
#' @importFrom reshape2 melt
#'
NULL

#' Create a rank summary
#'
#' This function creates a summary of multiple ranks provided for input items.
#'
#' @param df A data frame with ranks as columns and items as rows.
#'
#' @return A rank summary data frame with three columns: 'Rank', 'Item' and
#' 'Count'.
#'
#' @examples
#' df <- do.call(cbind, lapply(seq(30), function(i) sample(10, 10)))
#' rownames(df) <- paste0('M', seq(10))
#' colnames(df) <- paste0('R', seq(30))
#' rankSummary(df)
#'
#' @export
#'
rankSummary <- function(df){
    minPos <- min(df)
    maxPos <- max(df)
    positions <- sort(unique(as.numeric(df)))
    posDF <- do.call(rbind, lapply(positions, function(pos)
        apply(df, 1, function(x) sum(x == pos))))
    rownames(posDF) <- positions
    smr <- reshape2::melt(posDF)
    colnames(smr) <- c('Rank', 'Item', 'Count')
    smr <- smr[order(smr$Rank, -smr$Count), ]
    smr$Rank <- factor(smr$Rank)
    return(smr)
}

#' Compute the average rank of each item
#'
#' This function computes the average rank of each item.
#'
#' @param rankDF Rank data frame created with \code{rankSummary}.
#' @param sigDigits Number of significant digits to use for the mean ranks.
#'
#' @return A single-column data frame of average ranks.
#'
#' @noRd
#'
computeMeanRanks <- function(rankDF, sigDigits=2){
    if (is.null(sigDigits))
        sigDigits <- 2
    nMetrics <- sum(rankDF[, 3]) / length(unique(rankDF[, 2]))
    meanRanks <- sort(vapply(as.character(unique(rankDF[, 2])), function(x){
        subRankDF <- rankDF[rankDF[, 2] == x, ]
        return(round(sum(as.numeric(as.character(subRankDF[, 1])) *
                             subRankDF[, 3]) / nMetrics, sigDigits))},
        numeric(1)))
    return(data.frame(Item = names(meanRanks), MeanRank = meanRanks))
}

#' Create a rank plot
#'
#' This function creates a rank plot.
#'
#' @inheritParams documentFun
#' @param df A data frame with ranks as columns and items as rows, or a
#' summary data frame generated with \code{rankSummary}. If the latter,
#' \code{summarize} must be set to \code{FALSE}.
#' @param summarize Whether to summarize the ranks with \code{rankSummary}.
#' Must be set to \code{FALSE} if the input data frame has been generated with
#' \code{rankSummary}.
#' @param xLab Label of x axis.
#' @param sigDigits Number of significant digits used when displaying mean
#' ranks. If \code{NULL}, the mean ranks will not be displayed.
#' @param labelSize Size of label marking average rank for each item.
#' Ignored if \code{sigDigits} is \code{NULL}.
#' @param labelColor Color of label marking average rank for each item.
#' Ignored if \code{sigDigits} is \code{NULL}.
#' @param labelFace Font face of label marking average rank for each item. Must
#' be one among 'plain', 'bold', 'italic' and 'bold-italic'.
#' Ignored if \code{sigDigits} is \code{NULL}.
#' @param labelScalingFactor Scaling factor used when displaying mean ranks.
#' Ignored if \code{sigDigits} is \code{NULL}.
#' @param labelOffset Vertical offset used when displaying mean ranks.
#' Ignored if \code{sigDigits} is \code{NULL}.
#'
#' @return An object of class \code{gg}.
#'
#' @examples
#' df <- do.call(cbind, lapply(seq(30), function(i) sample(10, 10)))
#' rownames(df) <- paste0('M', seq(10))
#' colnames(df) <- paste0('R', seq(30))
#' rankPlot(df)
#'
#' @export
#'
rankPlot <- function(df,
                     title = NULL,
                     summarize = TRUE,
                     viridisPal = 'turbo',
                     xLab = 'Item',
                     yLab = 'Rank count',
                     legendTitle = 'Rank',
                     sigDigits = NULL,
                     labelSize = 2.5,
                     labelColor = 'black',
                     labelFace = c('plain', 'bold', 'italic', 'bold-italic'),
                     legendTextSize = 10,
                     legendTitleSize = 10,
                     axisTextSize = 12,
                     axisTitleSize = 12,
                     xAngle = 45,
                     vJust = 0.6,
                     labelScalingFactor = 0.9,
                     labelOffset = 0.05,
                     ...){

    if(summarize)
        df <- rankSummary(df)

    meanRanks <- computeMeanRanks(df, sigDigits)
    itemOrder <- rownames(meanRanks)
    df[, 2] <- factor(df[, 2], levels=itemOrder)

    p <- ggplot() +
        geom_bar(aes(x=.data[[names(df)[2]]],
                     y=.data[[names(df)[3]]],
                     fill=.data[[names(df)[1]]]), df, stat='identity') +
        theme_classic() +
        scale_fill_viridis_d(option=viridisPal) + labs(x=xLab,
                                                       y=yLab,
                                                       fill=legendTitle) +
        theme(legend.text=element_text(size=legendTextSize),
              legend.title=element_text(size=legendTitleSize),
              axis.text.x=element_text(angle=xAngle, vjust=vJust),
              axis.text=element_text(size=axisTextSize),
              axis.title=element_text(size=axisTitleSize))

    if(!is.null(sigDigits)){
        labelFace <- match.arg(labelFace,
                               c('plain', 'bold', 'italic', 'bold-italic'))
        nRanks <- sum(df[, 3]) / length(unique(df[, 1]))
        labelHeights <- labelScalingFactor * nRanks *
            (liver::minmax(meanRanks[, 2]) + labelOffset)
        p <- p + geom_label(data=meanRanks,
                            aes(x=.data[[names(meanRanks)[1]]],
                               y=labelHeights,
                               label=.data[[names(meanRanks)[2]]]),
                            size=labelSize,
                            color=labelColor,
                            fontface=labelFace)
    }

    p <- centerTitle(p, title, ...)
    return(p)
}
