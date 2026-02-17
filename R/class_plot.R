#' @importFrom rlang .data
#'
NULL

#' Plot bars for item counts grouped and colored by class
#'
#' This function plots bars for item counts grouped and colored by class.
#'
#' @inheritParams documentFun
#' @param df A data frame with at least three columns. Its first column
#' (categorical) colors the plot bars. The second column (categorical)
#' labels the plots bars. The third column (numeric) sets the bar lengths.
#' @param decreasing Whether to display the bars in decreasing order of length.
#' @param valueCutoff Cutoff used for filtering the input data frame based on
#' the third (value) column. Only values above this cutoff will be displayed on
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
                      title = NULL,
                      xLab = 'Value',
                      yLab = 'Item',
                      legendTitle = 'Class',
                      palette = 'Spectral',
                      labelSize = 2.5,
                      labelColor ='black',
                      legendTitleSize = 10,
                      legendTextSize = 10,
                      axisTextSize = 12,
                      axisTitleSize = 12,
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
        theme(legend.text=element_text(size=legendTextSize),
              legend.title=element_text(size=legendTitleSize),
              axis.ticks.y=element_blank(),
              axis.text.y=element_blank(),
              axis.text.x=element_text(size=axisTextSize),
              axis.title.x=element_text(size=axisTitleSize)) +
        labs(x=xLab, y=yLab, fill=legendTitle) +
        scale_fill_manual(values=hcl.colors(nClasses, palette)) +
        geom_text(aes(x=.data[[names(df)[3]]] / 2,
                      y=.data[[names(df)[4]]],
                      label=.data[[names(df)[2]]]),
                  size=labelSize,
                  color=labelColor)
    p <- centerTitle(p, title, ...)
    return(p)
}
