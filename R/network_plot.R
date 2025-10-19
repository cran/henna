#' @importFrom dplyr mutate
#' @importFrom grDevices chull hcl.colors
#' @importFrom ggraph geom_edge_link geom_node_point geom_node_text ggraph scale_edge_width
#' @importFrom tidygraph activate as_tbl_graph
#'
NULL

#' Prepare data frame for network plot
#'
#' This function prepares a data frame for network plot.
#'
#' @param df Data frame.
#' @param rankCol Name of the rank column.
#' @param edgeScale Scaling factor used in generating
#' edge weights.
#'
#' @return A data frame ready to serve as input to \code{networkPlot}.
#'
#' @keywords internal
#'
networkPlotDF <- function(df,
                          rankCol = 'rank',
                          edgeScale = 2){
    preWeight <- log(max(df[[rankCol]]) /
                         df[[rankCol]] + 0.01)
    df$weight <- edgeScale * preWeight /
        max(preWeight)
    df <- df[, c('gene1', 'gene2', 'weight')]
    return(df)
}

#' Plot graph with the option of using different colors for connected components
#'
#' This function plots the graph of the data frame and optionallt uses different
#' colors for nodes belonging to different connected components.
#'
#' @inheritParams networkPlotDF
#' @inheritParams riverPlot
#' @param nodeSize Size of graph nodes.
#' @param nodeTextSize Size of text on graph nodes.
#' @param palette grDevices palette used for coloring nodes.
#' Ignored if nodeColor is not \code{NULL}.
#' @param nodeColor Color used for nodes. Default is \code{NULL}
#' (\code{palette} will instead be used).
#' @param edgeColor Color used for edges.
#'
#' @return An object of class \code{ggraph}.
#'
#' @examples
#' df <- data.frame(gene1 = paste0('G', c(1, 2, 5, 6, 7, 17)),
#' gene2 = paste0('G', c(2, 5, 8, 11, 11, 11)),
#' rank = c(1, 1, 3, 3, 3, 3))
#' networkPlot(df)
#'
#' @export
#'
networkPlot <- function(df, title = 'Network plot',
                        rankCol = 'rank',
                        edgeScale = 2,
                        nodeSize = 10,
                        nodeTextSize = 2.3,
                        palette = 'Spectral',
                        nodeColor = NULL,
                        edgeColor = 'black',
                        ...){
    df <- networkPlotDF(df, rankCol, edgeScale)
    tblGraph <- as_tbl_graph(df, directed=FALSE)
    df <- connectedComponents(df)
    vertexComp <- vertexComponents(df)
    nComp <- length(unique(vertexComp))
    tblGraph <- mutate(activate(tblGraph, "vertices"),"component" = vertexComp)
    p <- ggraph(tblGraph, layout="nicely") +
        geom_edge_link(aes(width=.data[["weight"]]), color=edgeColor) +
        scale_edge_width(range=c(0.1, 0.3)) +
        theme_void() +
        theme(legend.position='none') +
        scale_color_manual(values=hcl.colors(nComp, palette))
    if (is.null(nodeColor))
        p <- p + geom_node_point(aes(color=.data[["component"]]),
                                 size=nodeSize) else
                                 p <- p + geom_node_point(color=nodeColor,
                                                          size=nodeSize)
    p <- p + geom_node_text(aes(label=.data[["name"]]), size=nodeTextSize)
    p <- centerTitle(p, title, ...)
    return(p)
}
