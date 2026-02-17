# Script to generate the figures in man/figures
#'
createPlot1 <- function(){
    if (requireNamespace(c('ggplot2', 'henna', 'qs2'),
                         quietly=TRUE)){
        df <- qs2::qs_read('inst/extdata/classPlot.qs2')
        p <- henna::classPlot(df,
                              title='Most abundant cell types grouped by donor',
                              xLab='Count',
                              yLab='Cell type',
                              legendTitle='Donor',
                              labelSize=2.1,
                              valueCutoff=100)
        ggplot2::ggsave("inst/figures/class_plot.png", p, width=6, height=4)
    }
}

createPlot2 <- function(){
    if (requireNamespace(c('ggplot2', 'henna', 'qs2'),
                         quietly=TRUE)){
        corMat <- qs2::qs_read('inst/extdata/correlationPlot.qs2')
        p <- henna::correlationPlot(corMat, labelSize=2.5)
        ggplot2::ggsave("inst/figures/correlation_plot.png", p, width=6,
                        height=4)
    }
}

createPlot3 <- function(){
    if (requireNamespace(c('ggplot2', 'henna', 'qs2'),
                         quietly=TRUE)){
        df <- qs2::qs_read('inst/extdata/densityPlot.qs2')
        p <- henna::densityPlot(df, 'Density plot', colorScheme='sea',
                                labelSize=2)
        ggplot2::ggsave("inst/figures/density_plot.png", p, width=6,
                        height=4)
    }
}

createPlot4 <- function(){
    if (requireNamespace(c('ggplot2', 'henna', 'qs2'),
                         quietly=TRUE)){
        sharedDF <- qs2::qs_read('inst/extdata/hullPlot.qs2')
        name1 <- 'alpha'
        name2 <- 'delta'
        legendLabs <- as.factor(c('Non-top',
                               'Shared',
                               paste0('Top only for ', name2),
                               paste0('Top only for ', name1)))
        p <- henna::hullPlot(sharedDF,
                             'Shared markers plot',
                             xInt=1.5,
                             yInt=1.3,
                             hullWidth=0.5,
                             labXThr=1.5,
                             labYThr=1.3,
                             xLab=paste0('avg_log2FC (', name1, ')'),
                             yLab=paste0('avg_log2FC (', name2, ')'),
                             legendLabs=legendLabs)
        ggplot2::ggsave("inst/figures/hull_plot.png", p, width=6,
                        height=4)
    }
}

createPlot5 <- function(){
    if (requireNamespace(c('ggplot2', 'henna', 'qs2'), quietly=TRUE)){
        overlapDF <- qs2::qs_read('inst/extdata/networkPlot.qs2')
        p <- henna::networkPlot(overlapDF,
                                'Genes with strongly overlapping expression',
                                'rank',
                                'ranks')
        ggplot2::ggsave("inst/figures/network_plot.png", p, width=6,
                        height=4)
    }
}

createPlot6 <- function(){
    if (requireNamespace(c('ggplot2', 'henna', 'qs2'), quietly=TRUE)){
        degreesDF <- qs2::qs_read('inst/extdata/radialPlot.qs2')
        p <- henna::radialPlot(degreesDF,
                               title='Genes involved in top overlaps',
                               groupLegendTitle='Component',
                               valueLegendTitle='Degree',
                               extraCircles=2)
        ggplot2::ggsave("inst/figures/radial_plot.png", p, width=6,
                        height=4)
    }
}

createPlot7 <- function(){
    if (requireNamespace(c('ggplot2', 'henna', 'qs2'), quietly=TRUE)){
        ranksDF <- qs2::qs_read('inst/extdata/rankPlot.qs2')
        p <- henna::rankPlot(ranksDF,
                             paste0('Marker ranks for alpha, beta, delta ',
                                    'and gamma cells'),
                             sigDigits=2,
                             xLab='Gene')
        ggplot2::ggsave("inst/figures/rank_plot.png", p, width=6,
                        height=4)
    }
}

createPlot8 <- function(){
    if (requireNamespace(c('ggplot2', 'henna', 'qs2'), quietly=TRUE)){
        riverDF <- qs2::qs_read('inst/extdata/riverPlot.qs2')
        p <- henna::riverPlot(riverDF,
                              paste0('Overlaps between cell type markers ',
                                     'and donor markers'))
        ggplot2::ggsave("inst/figures/river_plot.png", p, width=6,
                        height=4)
    }
}

createPlot9 <- function(){
    if (requireNamespace(c('ggplot2', 'henna', 'qs2'), quietly=TRUE)){
        mat <- qs2::qs_read('inst/extdata/tilePlot.qs2')
        p <- henna::tilePlot(mat,
                             title='Number of shared markers',
                             xLab=NULL,
                             yLab=NULL)
        ggplot2::ggsave("inst/figures/tile_plot.png", p, width=6, height=4)
    }
}

createPlot10 <- function(){
    if (requireNamespace(c('ggplot2', 'henna', 'qs2'), quietly=TRUE)){
        df <- qs2::qs_read('inst/extdata/volcanoPlot.qs2')
        p <- henna::volcanoPlot(df,
                                title='Volcano plot - beta cells',
                                pvalThr=1e-10,
                                logFCThr=1,
                                labPvalThr=1e-150,
                                labLogFCThr=5.3)
        ggplot2::ggsave("inst/figures/volcano_plot.png", p, width=6, height=4)
    }
}

createFigures <- function()
    for (i in seq(10))
        eval(as.name(paste0('createPlot', i)))()

createFigures()
