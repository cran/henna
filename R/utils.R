#'@importFrom abdiv euclidean
#'@importFrom ggplot2 aes after_stat coord_fixed element_blank element_text expand_limits geom_bar geom_label geom_point geom_polygon geom_segment geom_text geom_tile ggplot ggproto ggtitle labs margin scale_color_discrete scale_color_manual scale_fill_gradientn scale_fill_manual scale_fill_viridis_d stat_density_2d theme theme_bw theme_classic theme_linedraw theme_minimal theme_void xlab
#'@importFrom stats density runif setNames
#'
NULL

#' Add a centered title to a plot
#'
#' This function adds a centered title to a ggplot object.
#'
#' @param p A ggplot object.
#' @param title Plot title.
#' @param ... Other arguments passed to \code{ggplot2::element_text}.
#'
#' @return A ggplot object.
#'
#' @export
#'
centerTitle <- function(p, title, ...)
    return(p + ggtitle(title) + theme(plot.title=element_text(hjust=0.5, ...)))

#' Add a nearest neighbor column to a data frame of points
#'
#' This function adds a nearest neighbor column to a data frame of points.
#'
#' @inheritParams dfPoint
#'
#' @return A data frame with an added nearest neighbor column.
#'
#' @noRd
#'
addNNCol <- function(df, pointCoords = c(1, 2)){
    df$nn <- vapply(rownames(df), function(i)
        names(which.min(vapply(rownames(df), function(j)
            dfEuclidean(df, i, j, pointCoords, replaceZero=Inf),
            numeric(1)))), character(1))
    return(df)
}

#' Extract point from data frame of points.
#'
#' This function extracts a point from a data frame of points.
#'
#' @param df A data frame containing the coordinates
#' of the points.
#' @param i Row name or index.
#' @param pointCoords The row names or indices of point coordinates.
#'
#' @return A numeric vector containing the point coordinates.
#'
#' @keywords internal
#'
dfPoint <- function(df, i, pointCoords = c(1, 2))
    return(unlist(df[i, pointCoords]))

#' Find the Euclidean distance between two points in a data frame
#'
#' This function finds the Euclidean distance between two points in a data
#' frame.
#'
#' @inheritParams dfPoint
#' @param j Row name or index
#' @param replaceZero Value to replace zero with. If set to 0, no replacement
#' will occur.
#'
#' @return A numeric value.
#'
#' @keywords internal
#'
dfEuclidean <- function(df, i, j, pointCoords = c(1, 2), replaceZero = 0){
    eDist <- euclidean(dfPoint(df, i, pointCoords),
                       dfPoint(df, j, pointCoords))
    if (!eDist)
        return(replaceZero)
    return(eDist)
}

#' Expand range of a vector
#'
#' This function expands the range of a vector by adding a new minimum and
#' maximum.
#'
#' @param v A numeric vector.
#' @param expandPerc Percentage by which to expand the vector.
#'
#' @return A numeric vector of size 2, containing the new minimum and maximum.
#'
#' @noRd
#'
expandRange <- function(v, expandPerc = 10){
    vMin <- min(v)
    vMax <- max(v)
    vRange <- vMax - vMin
    expVal <- vRange * expandPerc / 100
    return(c(vMin - expVal, vMax + expVal))
}
