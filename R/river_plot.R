#'@importFrom ggalluvial geom_alluvium geom_stratum StatStratum
#'
NULL

#' Create an alluvial plot
#'
#' This function creates an alluvial plot.
#'
#' @inheritParams documentFun
#' @param df A data frame with two categorical columns and a numeric column.
#' @param fillColIndex Index of column used for coloring the alluvia.
#' @param curveType Curve type.
#' @param alpha Opaqueness level for the colors of the alluvia.
#' @param strataFill Color used for the strata.
#' @param labelSize Size of labels of strata elements.
#' @param ... Other arguments passed to \code{centerTitle}.
#'
#' @return An object of class \code{gg}.
#'
#' @examples
#' df <- data.frame(x = sample(c('a','b', 'c', 'd', 'e', 'f'), 20,
#' replace=TRUE),
#' y = sample(c('p','q', 'r', 's', 't', 'u', 'v', 'w'), 20,
#' replace=TRUE),
#' z = runif(20, 1, 3))
#' riverPlot(df)
#'
#' @export
#'
riverPlot <- function(df,
                      title = NULL,
                      fillColIndex = 2,
                      curveType = 'sigmoid',
                      alpha = 0.8,
                      strataFill = 'lightgoldenrod1',
                      labelSize = 3,
                      axisTextSize = 12,
                      axisTitleSize = 12,
                      viridisPal = 'turbo',
                      ...){
    p <- ggplot(data=df, aes(axis1=.data[[names(df)[1]]],
                             axis2=.data[[names(df)[2]]],
                             y=.data[[names(df)[3]]])) +
        geom_alluvium(aes(fill=.data[[names(df)[fillColIndex]]]),
                      curve_type=curveType,
                      alpha=alpha) +
        geom_stratum(fill=strataFill) +
        geom_text(stat=StatStratum, aes(label=after_stat(stratum)),
                  size=labelSize) +
        scale_fill_viridis_d(option=viridisPal) +
        theme_void() +
        theme(legend.position='none')
    p <- centerTitle(p, title, ...)
    return(p)
}
