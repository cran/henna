#' Create a palette designed for densityPlot
#'
#' This function returns a palette designed for \code{densityPlot}.
#'
#' @param palette One of 'lava', 'oasis', 'orichalc', 'sea' and 'sky'.
#' @return A character vector of colors.
#'
#' @examples
#' dpColors('sea')
#'
#' @export
#'
dpColors <- function(palette = c('cake', 'cloudy', 'grapes', 'lava', 'oasis',
                                 'orichalc', 'sea','sky')){
    palette <- match.arg(palette, c('cake', 'cloudy', 'grapes', 'lava', 'oasis',
                                    'orichalc', 'sea','sky'))
    palList <- list(cake = c('orangered4', 'brown4','tomato4',
                             'coral4', 'lightyellow', 'snow',
                             'mistyrose', 'red2', 'firebrick3'),
                    cloudy = c('white', 'snow', 'snow1', 'lightblue1',
                               'lightskyblue1', 'lightskyblue', 'skyblue1',
                               'deepskyblue', 'deepskyblue1'),
                    grapes = c('olivedrab1', 'darkolivegreen1', 'mistyrose1',
                               'lightpink','palevioletred', 'palevioletred4',
                               'orchid4', 'darkorchid4', 'purple4'),
                    lava = c('bisque4','bisque3','bisque2',
                              'bisque1','bisque','lightyellow',
                              'goldenrod1','red2','red3'),
                    oasis = c('navajowhite1','bisque1','wheat1',
                              'darkolivegreen1','chartreuse','green',
                              'cadetblue1', 'cyan2', 'deepskyblue'),
                    orichalc = c('dodgerblue3','dodgerblue2', 'deepskyblue',
                                 'cyan2', 'honeydew1', 'lightgoldenrodyellow',
                                 'lemonchiffon','wheat','red'),
                    sea = c('midnightblue','dodgerblue4','dodgerblue3',
                            'dodgerblue2','deepskyblue','cyan2',
                            'lightgoldenrodyellow','darkolivegreen1','green'),
                    sky = c('deepskyblue', 'deepskyblue1', 'skyblue1',
                            'lightskyblue','lightskyblue1', 'lightblue1',
                            'snow', 'snow1', 'snow2'))
    return(palList[[palette]])
}

#' Return default label, point and segment colors for the dpColors palettes
#'
#' This function returns default label, point and segment colors for
#' the \code{dpColors} palettes.
#'
#' @inheritParams dpColors
#' @return A data frame of colors.
#'
#' @noRd
#'
lpsColors <- function(){
    df <- data.frame(cake = c('black', 'blue', 'deepskyblue'),
                     cloudy = c('black', 'red', 'plum1'),
                     grapes = c('black', 'blue', 'deepskyblue'),
                     lava = c('black', 'black', 'dodgerblue3'),
                     oasis = c('black', 'red', 'mediumpurple'),
                     orichalc = c('black', 'black', 'lightpink1'),
                     sea = c('black', 'red', 'plum1'),
                     sky = c('black', 'red', 'thistle'))
    rownames(df) <- c('label', 'point', 'segment')
    return(df)
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

