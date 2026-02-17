#' @importFrom dplyr mutate
#' @importFrom grDevices chull hcl.colors
#' @importFrom ggraph geom_edge_link geom_node_point geom_node_text ggraph scale_edge_width
#' @importFrom paletteer paletteer_c
#' @importFrom tidygraph activate as_tbl_graph
#'
NULL

#' Prepare data frame for network plot
#'
#' This function prepares a data frame for network plot.
#'
#' @param df Data frame.
#' @param rankCol Name of the rank column.
#'
#' @return A data frame ready to serve as input to \code{networkPlot}.
#'
#' @keywords internal
#'
networkPlotDF <- function(df,
                          rankCol = 'rank'){
    preWeight <- log(max(df[[rankCol]]) /
                         df[[rankCol]] + 0.01)
    df$weight <- preWeight / max(preWeight)
    return(df)
}

#' Plot graph with the option of using different colors for connected components
#'
#' This function plots the graph of the data frame and optionally uses different
#' colors for nodes belonging to different connected components.
#'
#' @inheritParams documentFun
#' @inheritParams networkPlotDF
#' @param numCol Name of the numeric column used to vary edge widths. If no
#' such column is provided, set to \code{NULL}.
#' @param numColType The type of the numeric column used to vary edge widths.
#' Choose between 'weights' and 'ranks' Ignored if \code{numCol}
#' is \code{NULL}.
#' @param nodeSize Size of graph nodes.
#' @param nodeTextSize Size of text on graph nodes.
#' @param palette grDevices palette used for coloring nodes.
#' Ignored if nodeColor is not \code{NULL}.
#' @param nodeColor Color used for nodes. Default is \code{NULL}
#' (\code{palette} will instead be used).
#' @param edgeWidth Width to be used for all edges before scaling
#' if \code{numCol} is \code{NULL}; otherwise ignored.
#' @param edgeColor Color used for edges.
#' @param edgeScales Edge width scales. Must be a numeric vector of size
#' 2 (minimum and maximum).
#'
#' @return An object of class \code{ggraph}.
#'
#' @examples
#' df <- data.frame(gene1 = paste0('G', c(1, 2, 5, 6, 7, 17)),
#' gene2 = paste0('G', c(2, 5, 8, 11, 11, 11)),
#' rank = c(1, 1, 3, 3, 3, 3))
#' networkPlot(df, numCol='rank', numColType='ranks')
#'
#' @export
#'
networkPlot <- function(df,
                        title = NULL,
                        numCol = NULL,
                        numColType = c('weights', 'ranks'),
                        nodeSize = 10,
                        nodeTextSize = 2.3,
                        palette = 'grDevices::Spectral',
                        nodeColor = NULL,
                        edgeWidth = 1,
                        edgeColor = 'black',
                        edgeScales = c(0.2, 0.6),
                        ...){


    if (length(edgeScales) != 2)
        stop('`edgeScales` must be a numeric vector of length 2.')

    if(!is.null(numCol)){
        if (!numCol %in% colnames(df))
            stop('The edge width column was not found. ',
                 'If there is no edge width column, set `numCol` to NULL.')

        numColType <- match.arg(numColType, c('weights', 'ranks'))
        if (numColType == 'ranks')
            df <- networkPlotDF(df, numCol) else
                colnames(df)[which(colnames(df) %in% numCol)] <- 'weight'
    }

    tblGraph <- as_tbl_graph(df, directed=FALSE)
    df <- connectedComponents(df)
    vertexComp <- vertexComponents(df)
    nComp <- length(unique(vertexComp))
    tblGraph <- mutate(activate(tblGraph, 'vertices'),'component'=vertexComp)

    p <- ggraph(tblGraph, layout='nicely') +
        geom_edge_link(aes(width=if (is.null(numCol))
                                     edgeWidth else .data[['weight']]),
                       color=edgeColor) +
        scale_edge_width(range=edgeScales) +
        theme_void() +
        theme(legend.position='none') +
        scale_color_manual(values=paletteer_c(palette, nComp))

    if (is.null(nodeColor))
        p <- p + geom_node_point(aes(color=.data[['component']]),
                                 size=nodeSize) else
                                 p <- p + geom_node_point(color=nodeColor,
                                                          size=nodeSize)

    p <- p + geom_node_text(aes(label=.data[['name']]), size=nodeTextSize)
    p <- centerTitle(p, title, ...)
    return(p)
}
