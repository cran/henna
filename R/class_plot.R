#' @importFrom rlang .data
#'
NULL

#' Plot item bars grouped by class
#'
#' This function plots bars for each item while grouping them by class and
#' ordering them.
#'
#' @inheritParams networkPlot
#' @inheritParams hullPlot
#' @inheritParams radialPlot
#' @param df A data frame with at least three columns, with class, item and
#' value as the first three columns. The latter must be numeric.
#' @param legendLab Legend label.
#' @param labelColor Label color.
#' @param decreasing Whether to display the bars in decreasing order of length.
#' @param valueCutoff Cutoff used for filtering the input data frame based on
#' the value column. Only values greater than this cutoff will be displayed on
#' the plot.
#'
#' @return An object of class \code{gg}.
#'
#' @examples
#'  df <- data.frame(Class = sample(paste0('C', seq(13)), 25, replace=TRUE),
#'  Item = paste0('I', seq(25)),
#'  Value = runif(25, 0.5, 1))
#'  classPlot(df)
#'
#'  df <- data.frame(Class = sample(paste0('C', seq(13)), 25, replace=TRUE),
#'  Item = sample(paste0('I', seq(21)), 25, replace=TRUE),
#'  Value = runif(25, 0.5, 1))
#'  classPlot(df)
#'
#' @export
#'
classPlot <- function(df,
                      title = 'Class plot',
                      xLab = 'Value',
                      yLab = 'Item',
                      legendLab ='Class',
                      palette = 'Spectral',
                      labelSize = 2.5,
                      labelColor ='black',
                      decreasing = TRUE,
                      valueCutoff = 0,
                      ...){
    df <- df[df[, 3] > valueCutoff, ]
    nClasses <- length(unique(df[, 1]))
    df <- df[order(df[, 3], decreasing=decreasing), ]
    df[, 1] <- factor(df[, 1], levels=unique(df[, 1]))
    df[, 4] <- make.unique(as.character(df[, 2]))
    df[, 4] <- factor(df[, 4], levels=rev(df[, 4]))
    p <- ggplot(data=df,
                aes(fill=.data[[names(df)[1]]],
                    x=.data[[names(df)[3]]],
                    y=.data[[names(df)[4]]])) +
        geom_bar(position='stack', stat='identity') +
        theme_classic() +
        theme(axis.ticks.y=element_blank(),
              axis.text.y=element_blank()) +
        labs(x=xLab, y=yLab, fill=legendLab) +
        scale_fill_manual(values=hcl.colors(nClasses, palette)) +
        geom_text(aes(x=.data[[names(df)[3]]] / 2,
                      y=.data[[names(df)[4]]],
                      label=.data[[names(df)[2]]]),
                  size=labelSize,
                  color=labelColor)
    p <- centerTitle(p, title, ...)
    return(p)
}
