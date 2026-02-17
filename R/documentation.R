#' Internal function used for documenting other functions
#'
#' This function is used internally to help document other functions.
#'
#' @param title Plot title.
#' @param xLab x axis label.
#' @param yLab y axis label.
#' @param legendTitle Legend title.
#' @param legendLabs Legend labels.
#' @param legendPos Legend position.
#' @param palette Color palette.
#' @param viridisPal Viridis palette.
#' @param labeledPoints Point labels to be displayed on the plot.
#' @param labelType Whether to draw a box around labels (option 'boxed') or not
#' (option 'free'). Default is 'free'.
#' @param labelSize Label size.
#' @param labelColor Label color.
#' @param labelRepulsion Repulsion strength between labels.
#' @param labelPull Attraction strength between a text label
#' and its data point.
#' @param maxOverlaps Maximum number of allowed overlaps.
#' @param boxPadding Amount of padding around box.
#' @param labelPadding Amount of padding around label.
#' @param labelSegWidth Thickness of segment connecting label to point.
#' @param pointSize Point size.
#' @param pointShape Point shape.
#' @param legendTitleSize Legend title size.
#' @param legendTextSize Legend text size.
#' @param axisTextSize Axis text size.
#' @param axisTitleSize Axis title size.
#' @param xAngle Angle of x axis text.
#' @param vJust Vertical justification in [0, 1].
#' @param margins Plot margins. Must be a vector of size 4 listing the desired
#' top, right, bottom and left margin, in that order.
#' @param theme Plot theme.
#' @param pvalOffset Offset added to p-values to avoid infinite values when
#' taking logarithms.
#' @param ... Additional arguments passed to \code{centerTitle}.
#'
#' @return \code{NULL}. This function is only used internally for
#' documentation.
#'
#' @keywords internal

documentFun <- function(title = NULL,
                        xLab = 'x',
                        yLab = 'y',
                        legendTitle = 'Legend',
                        legendLabs = c('a', 'b'),
                        legendPos = 'right',
                        palette = 'Spectral',
                        viridisPal = 'turbo',
                        labeledPoints = NULL,
                        labelType = 'free',
                        labelOutside = TRUE,
                        labelSize = 2.5,
                        labelColor ='black',
                        labelRepulsion = 1,
                        labelPull = 1,
                        maxOverlaps = 50,
                        boxPadding = 0.2,
                        labelPadding = 0.1,
                        labelSegWidth = 0.4,
                        pointSize = 0.8,
                        pointShape = 1,
                        legendTitleSize = 10,
                        legendTextSize = 10,
                        axisTextSize = 12,
                        axisTitleSize = 12,
                        xAngle = 45,
                        vJust = 0.6,
                        margins = margin(0, -10, -10, -10),
                        theme = 'linedraw',
                        pvalOffset = 1e-317,
                        ...){}
