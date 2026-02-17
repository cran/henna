#' Find the connected components of a graph represented as a data frame
#'
#' This function finds the connected components of a graph represented as
#' a data frame.
#'
#' @param df A data frame with two categorical columns representing the edges
#' of a graph.
#' @param colName Name of the connected components column to be added.
#' @return A data frame with a column indicating the connected component of
#' each edge.
#'
#' @examples
#' df <- data.frame(
#' gene1 = paste0('G', c(1, 2, 6, 7, 8, 9,
#' 11, 25, 32, 17, 18)),
#' gene2 = paste0('G', c(2, 8, 8, 8, 1, 25,
#' 32, 24, 24, 26, 26))
#' )
#' connectedComponents(df)
#'
#' @export
#'
connectedComponents <- function(df, colName = 'component'){
    if(!nrow(df))
        stop('The dataframe has no rows.')
    df[[colName]] <- -1
    rownames(df) <- seq(dim(df)[1])
    vertices <- unique(c(df[, 1], df[, 2]))
    seen <- c()
    nextComp <- 1
    for (v in vertices){
        if (v %in% seen)
            next
        currVertices <- c(v)
        while (length(currVertices)){
            v <- currVertices[1]
            leftdf <- subset(df, df[, 1] == v)
            rightdf <- subset(df, df[, 2] == v)
            seen <- c(seen, v)
            newEdges <- as.integer(c(rownames(leftdf), rownames(rightdf)))
            df[newEdges, colName] <- nextComp
            neighbors <- setdiff(c(leftdf[, 2], rightdf[, 1]),
                                 c(currVertices, seen))
            currVertices <- c(currVertices, neighbors)
            currVertices <- currVertices[-1]
        }
        nextComp <- nextComp + 1
    }
    df[, colName] <- factor(df[, colName])
    return(df)
}

#' Return the connected components of vertices
#'
#' This function returns the connected components of vertices from a graph
#' data frame in which edges have been assigned connected components.
#'
#' @param df A data frame with two categorical columns representing graph edges
#' and a connected components column.
#' @param colName Name of the connected components column.
#'
#' @return A factor vector representing the connected component of each
#' vertex.
#'
#' @examples
#' df <- data.frame(gene1 = c('A', 'B', 'C', 'A'),
#' gene2 = c('B', 'D', 'F', 'G'),
#' component = c(1, 1, 2, 1))
#' vertexComponents(df)
#'
#' @export
#'
vertexComponents <- function(df, colName = 'component'){
    vertices <- unique(c(df[, 1], df[, 2]))
    boxes <- lapply(unique(df[, colName]), function(x) {
        compDF <- df[df[, colName] == x, ]
        return(unique(c(compDF[, 1], compDF[, 2])))
    })
    nBoxes <- length(boxes)
    allocations <- unlist(lapply(seq(nBoxes), function(i)
        setNames(rep(i, length(boxes[[i]])),boxes[[i]])))
    vertexComp <- as.factor(allocations[vertices])
    return(vertexComp)
}

#' Construct the convex hull of a set of points
#'
#' This function constructs the convex hull of a set of points.
#'
#' @details The points must be provided as a data frame with two columns.
#'
#' @param pointsDF A data frame with the x and y coordinates of the points.
#' @param hullIndices Precalculated hull indices. Default is \code{NULL}: hull
#' indices are not provided, but they are calculated by \code{convexHull}.
#'
#' @return A data frame with two columns representing the points on the convex
#' hull.
#'
#' @examples
#' pointsDF <- data.frame(a = c(1, 2, 2, 3, 3, 4, 5, 6, 8, 6,
#' 7, 8, 6, 8, 10, 3, 1),
#' b = c(2, 3, 4, 8, 5, 6, 5, 4, 8, 11, 13, 14, 2, 1, 2, 14, 9))
#' hull <- convexHull(pointsDF)
#'
#' @export
#'
convexHull <- function(pointsDF, hullIndices=NULL){
    if(!is.null(hullIndices))
        hull <- pointsDF[hullIndices, c(1, 2)] else
            hull <- pointsDF[chull(pointsDF[, 1], pointsDF[, 2]), c(1, 2)]
    colnames(hull) <- c('x', 'y')
    return(hull)
}

#' Construct a data frame of segments from a data frame of points
#'
#' This function constructs a data frame of segments from a data frame of
#' points.
#'
#' @param pointsDF A data frame with the x and y coordinates of the points.
#' Each point must appear only once.
#' @param joinEnds Whether to join the last point with the first one.
#'
#' @return A data frame of segments represented using four columns
#' (\code{x}, \code{y}, \code{xEnd}, \code{yEnd}).
#'
#' @examples
#' pointsDF <- data.frame(x = c(1, 2, 4, 7, 10,
#' 12, 13, 15, 16),
#' y = c(1, 1, 2, 3, 3, 2, 1, 2, 1))
#'
#' hullIndices <- grDevices::chull(pointsDF[, 1], pointsDF[, 2])
#' hull <- convexHull(pointsDF, hullIndices)
#' pointsToSegments(hull)
#'
#' @export
#'
pointsToSegments <- function(pointsDF,
                             joinEnds = TRUE){
    df <- data.frame(x = pointsDF[seq_len(nrow(pointsDF) - 1), 1],
                     y = pointsDF[seq_len(nrow(pointsDF) - 1), 2],
                     xEnd = pointsDF[seq(2, nrow(pointsDF)), 1],
                     yEnd = pointsDF[seq(2, nrow(pointsDF)), 2])
    if(joinEnds)
        df <- rbind(df, c(df$xEnd[nrow(df)],
                          df$yEnd[nrow(df)],
                          df$x[1],
                          df$y[1]))
    return(df)
}

#' Check if a point is on a segment
#'
#' This function checks if a point P is on a segment AB.
#'
#' @param xPoint x coordinate of point P.
#' @param yPoint y coordinate of point P.
#' @param xStart x coordinate of point A.
#' @param yStart y coordinate of point A.
#' @param xEnd x coordinate of point B.
#' @param yEnd y coordinate of point B.
#'
#' @return Logical; whether the point is on the segment.
#'
#' @examples
#' isPointOnSeg(2, 3, 1, 2, 3, 4)
#' isPointOnSeg(2, 3, 1, 2, 3, 8)
#' isPointOnSeg(4, 5, 1, 2, 3, 4)
#'
#' @export
#'
isPointOnSeg <- function(xPoint, yPoint, xStart, yStart, xEnd, yEnd){
    if (xPoint < min(xStart, xEnd) | xPoint > max(xStart, xEnd) |
        yPoint < min(yStart, yEnd) | yPoint < min(yStart, yEnd))
        return(FALSE)
    slope <- (yEnd - yStart) / (xEnd - xStart)
    yIntercept <- yStart - slope * xStart
    return (is(all.equal(slope * xPoint + yIntercept, yPoint))[1] == 'logical')
}

#' Check if a point is on a polygon boundary
#'
#' This function checks if a point P is on a polygon boundary.
#'
#' @inheritParams isPointOnSeg
#' @param boundary A data frame with four columns representing segments
#' comprising the boundary.
#'
#' @return Logical; whether the point is on the boundary.
#'
#' @examples
#' pointsDF <- data.frame(x = c(1, 2, 4, 7, 10,
#' 12, 13, 15, 16),
#' y = c(1, 1, 2, 3, 3, 2,
#' 1, 2, 1))
#'
#' hullIndices <- grDevices::chull(pointsDF[, 1], pointsDF[, 2])
#' hull <- convexHull(pointsDF, hullIndices)
#' hullSegments <- pointsToSegments(hull)
#'
#' isPointOnBoundary(2, 3, hullSegments)
#'
#' @export
#'
isPointOnBoundary <- function(xPoint, yPoint, boundary)
    return(as.logical(max(apply(boundary, 1, function(x){
        v <- unlist(x)
        names(v) <- c()
        return(isPointOnSeg(xPoint, yPoint, v[1], v[2], v[3], v[4]))
    }))))
