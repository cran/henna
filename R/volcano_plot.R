#' Create a volcano plot
#'
#' This function creates a volcano plot for a data frame with a log column and
#' a p-value column. The gene names must be provided as row names.
#'
#' @details Users can input labeled genes in two ways:
#'
#' 1) By using p-value and log fold-change thresholds (the default option).
#'
#' 2) By inputting a list of labels.
#'
#' @inheritParams documentFun
#' @param df A data frame with rownames as genes, a log column and a
#' p-value column.
#' @param logCol Log column.
#' @param pvalCol P-value column.
#' @param logFCThr Threshold used to separate significant log values.
#' @param pvalThr Threshold used to separate significant p-values.
#' @param labeledGenes Gene labels to be displayed on the plot. Default is
#' \code{NULL}, entailing that gene labels will be displayed on the basis
#' of \code{labLogFCThr} and \code{labPvalThr}, if at least one of
#' them is not \code{NULL}.
#' @param labelOutside Display labels for points specified by
#' \code{labeledGenes} even if they fall outside the boundaries imposed by
#' \code{labLogFCThr} and \code{labPvalThr}. Ignored if \code{labeledGenes} is
#' \code{NULL} or both \code{labLogFCThr} and \code{labPvalThr} are \code{NULL}.
#' @param labLogFCThr Threshold used to plot gene labels based on log values.
#' Ignored if \code{labeledGenes} is not \code{NULL}.
#' @param labPvalThr Threshold used to plot gene labels based on p-values.
#' Ignored if \code{labeledGenes} is not \code{NULL}.
#' @param alpha Opaqueness level of point color.
#' @param theme Plot theme. Choose between 'bw', 'classic', 'linedraw'
#' and 'minimal'. Default is 'minimal'.
#' @param ... Additional arguments passed to
#' \code{EnhancedVolcano::EnhancedVolcano}.
#'
#' @return An object of class \code{gg}.
#'
#' @examples
#' if (requireNamespace("EnhancedVolcano", quietly=TRUE)){
#' filePath <- system.file('extdata', 'volcanoPlot.qs2', package='henna')
#' df <- qs2::qs_read(filePath)
#' p <- volcanoPlot(df, title='Volcano plot - beta cells', pvalThr=1e-10,
#' logFCThr=1,
#' labPvalThr=1e-150,
#' labLogFCThr=5.3)
#' }
#'
#' @export
#'
volcanoPlot <- function(df,
                        title = NULL,
                        logCol = 'avg_log2FC',
                        pvalCol = 'p_val_adj',
                        xLab = expression(log[2] ~ fold ~ change),
                        yLab = expression(-log[10] ~ `p-value`),
                        legendTitle = 'Significance',
                        legendLabs = c('Not significant',
                                       expression(log[2] ~ FC),
                                       'p-value',
                                       expression(`p-value` ~ and
                                                  ~ log[2] ~ FC)),
                        legendPos = c('right', 'top', 'left', 'bottom'),
                        logFCThr = 1,
                        pvalThr = 1e-05,
                        labeledGenes = NULL,
                        labelOutside = FALSE,
                        labLogFCThr = 1.8,
                        labPvalThr = 1e-12,
                        labelType = c('boxed', 'free'),
                        labelSize = 2.2,
                        labelColor = 'black',
                        labelRepulsion = 1,
                        labelPull = 0,
                        maxOverlaps = 100,
                        boxPadding = 0.2,
                        labelPadding = 0.1,
                        labelSegWidth = 0.4,
                        pointSize = 0.8,
                        alpha = 0.6,
                        palette = c("gray31", "goldenrod2", "orchid3",
                                    "red2"),
                        legendTextSize = 10,
                        legendTitleSize = 10,
                        axisTextSize = 12,
                        axisTitleSize = 12,
                        theme = c('minimal', 'bw', 'classic', 'linedraw'),
                        ...){

    if (!requireNamespace("EnhancedVolcano", quietly=TRUE))
        stop("You need to install the EnhancedVolcano package from Bioconductor
             to use volcanoPlot.")
    legendPos <- match.arg(legendPos, c('right', 'top', 'left', 'bottom'))
    theme <- match.arg(theme, c('minimal', 'bw', 'classic', 'linedraw'))
    labelType <- match.arg(labelType, c('boxed', 'free'))

    noGenes <- paste0(rep('#', max(vapply(rownames(df), nchar, integer(1)))
                          + 1), collapse='')

    p <- EnhancedVolcano::EnhancedVolcano(df,
                                          lab=rownames(df),
                                          x=logCol,
                                          y=pvalCol,
                                          title=NULL,
                                          xlab=xLab,
                                          ylab=yLab,
                                          subtitle=NULL,
                                          caption=NULL,
                                          pCutoff=pvalThr,
                                          FCcutoff=logFCThr,
                                          selectLab=noGenes,
                                          labSize=labelSize,
                                          pointSize=pointSize,
                                          legendLabels=legendLabs,
                                          col=palette,
                                          colAlpha=alpha,
                                          ...) +
        labs(color=legendTitle)

    labelDF <- createLabelDFVolcano(df, logCol, pvalCol, labeledGenes,
                                    labelOutside, labLogFCThr, labPvalThr)

    if(!is.null(labelDF))
        p <- labelPoints(p, labelDF, rownames(labelDF), labelType, labelSize,
                         labelColor, labelRepulsion, labelPull, maxOverlaps,
                         boxPadding,labelPadding, labelSegWidth)
    p <- p + eval(as.name(paste0('theme_', theme)))()
    p <- p + theme(legend.position=legendPos,
                   legend.text=element_text(size=legendTextSize),
                   legend.title=element_text(size=legendTitleSize),
                   axis.text=element_text(size=axisTextSize),
                   axis.title=element_text(size=axisTitleSize))
    p <- centerTitle(p, title)
    return(p)
}
