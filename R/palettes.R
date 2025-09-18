#' Create a palette designed for densityPlot
#'
#' This function returns a palette designed for \code{densityPlot}.
#'
#' @param palette One of 'sea' and 'lava'.
#' @return A character vector of colors.
#'
#' @export
#'
dpColors <- function(palette = c('sea', 'lava')){
    palette <- match.arg(palette, c('sea', 'lava'))
    if (palette == 'sea')
        return(c('midnightblue','dodgerblue4','dodgerblue3',
                 'dodgerblue2','deepskyblue','cyan2',
                 'lightgoldenrodyellow','darkolivegreen1','green'))
    if (palette == 'lava')
        return(c('bisque4','bisque3','bisque2',
                 'bisque1','bisque','lightyellow',
                 'goldenrod1','red2','red3'))
}

#' Create the default hullPlot palette
#'
#' This function returns the default palette used by \code{hullPlot}.
#'
#' @return A character vector of colors.
#'
#' @export
#'
hpColors <- function()
    return(c('gold','purple', 'blue', 'red'))


#' Create a palette designed to represent dots over a viridis background
#'
#' This function returns a 10-color palette used as the default
#' of \code{radialPlot}.
#'
#' @param nColors Number of colors.
#'
#' @return A character vector of colors.
#'
#' @export
#'
rpColors <- function(nColors = 10){
    colors <- c('red', 'purple1', 'olivedrab1','darkorange1',
                'lavender', 'thistle1','green1','violetred4',
                'goldenrod1', 'firebrick4')
    return(colors[seq(nColors)])
}

