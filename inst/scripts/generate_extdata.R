# A self-contained script to generate the data in inst/extdata
#'
createExtData <- function(){
    if (requireNamespace(c('CSOA', 'dplyr', 'hammers', 'LISTO', 'qs2',
                           'scRNAseq','scuttle','Seurat'), quietly=TRUE)){
        scObj <- scRNAseq::BaronPancreasData('human')
        scObj <- scuttle::logNormCounts(scObj)
        scObj <- Seurat::as.Seurat(scObj)

        df <- dplyr::count(scObj[[]], donor, label)
        qs2::qs_save(df, 'inst/extdata/classPlot.qs2')

        genes <- sort(c('FLNA','SERPING1', 'SERPINH1', 'CHGA', 'CHGB','COL1A1',
                        'COL6A1', 'CELA3B', 'CELA3A', 'CD24', 'KRT8', 'KRT18',
                        'NOTCH3'))
        mat <- hammers::scExpMat(scObj, genes=genes)
        corMat <- cor(t(mat))
        qs2::qs_save(corMat, 'inst/extdata/correlationPlot.qs2')

        genes <- c('COX7B', 'SMURF2', 'MINOS1','C19orf43', 'EIF3D', 'PTGES3',
                   'TMED4', 'ARL2', 'C12orf75', 'NUCKS1', 'RNF187',
                   'SMPD1', 'ACTR1A', 'TCEAL4', 'EID2B',
                   'COPRS', 'CDC42BPB', 'PDAP1', 'PRPF8', 'ATRAID', 'ILF3',
                   'CERS2', 'TBCK', 'FNDC3A', 'VAPB', 'AKAP9')
        mat <- hammers::scExpMat(scObj, genes=genes)
        d <- stats::dist(mat, 'euclidean')
        df <- data.frame(cmdscale(d))
        qs2::qs_save(df, 'inst/extdata/densityPlot.qs2')

        markers <- LISTO::buildMarkerList(scObj, 'label',
                                          ids1=c('alpha', 'beta',
                                                 'gamma', 'delta'))
        sharedDF <- LISTO::sharedMarkers(markers[[1]], markers[[4]])
        qs2::qs_save(sharedDF, 'inst/extdata/hullPlot.qs2')

        alphaMarkers <- c('GCG', 'TTR', 'PCSK2', 'FXYD5', 'LDB2', 'MAFB',
                          'CHGA', 'SCGB2A1', 'GLS', 'FAP', 'DPP4', 'GPR119',
                          'PAX6', 'NEUROD1', 'LOXL4', 'PLCE1', 'GC', 'KLHL41',
                          'FEV', 'PTGER3', 'RFX6', 'SMARCA1', 'PGR', 'IRX1',
                          'UCP2', 'RGS4', 'KCNK16', 'GLP1R', 'ARX', 'POU3F4',
                          'RESP18', 'PYY', 'SLC38A5', 'TM4SF4', 'CRYBA2', 'SH3GL2',
                          'PCSK1', 'PRRG2', 'IRX2', 'ALDH1A1','PEMT', 'SMIM24',
                          'F10', 'SCGN', 'SLC30A8')

        deltaMarkers <- c('SST', 'GABRB3', 'FRZB', 'MS4A8', 'BAIAP3', 'CASR', 'BCHE',
                          'UNC5B','EDN3', 'GHSR', 'PCSK1', 'GABRG2', 'POU3F1',
                          'BHLHE41', 'EHF', 'LCORL', 'ETV1', 'PDX1', 'LEPR', 'UCP2',
                          'NPTX2', 'FXYD2', 'IAPP', 'KCNK16', 'SCGN', 'ISL1', 'HHEX',
                          'RESP18', 'PAX4', 'RBP4', 'PCSK9', 'FFAR4')


        ductalMarkers <-  c('CFTR', 'SERPINA5', 'SLPI', 'TFF1', 'CFB', 'LGALS4',
                            'CTSH',	'PERP', 'PDLIM3',	'WFDC2', 'SLC3A1', 'AQP1',
                            'ALDH1A3', 'VTCN1',	'KRT19', 'TFF2', 'KRT7', 'CLDN4',
                            'LAMB3', 'TACSTD2', 'CCL2', 'DCDC2','CXCL2', 'CLDN10',
                            'HNF1B', 'KRT20', 'MUC1', 'ONECUT1', 'AMBP', 'HHEX',
                            'ANXA4', 'SPP1', 'PDX1', 'SERPINA3', 'GDF15', 'AKR1C3',
                            'MMP7', 'DEFB1', 'SERPING1', 'TSPAN8', 'CLDN1', 'S100A10',
                            'PIGR')

        gammaMarkers <- c('PPY', 'ABCC9', 'FGB', 'ZNF503', 'MEIS1', 'LMO3', 'EGR3',
                          'CHN2', 'PTGFR', 'ENTPD2', 'AQP3', 'THSD7A', 'CARTPT',
                          'ISL1', 'PAX6', 'NEUROD1', 'APOBEC2', 'SEMA3E', 'SLITRK6',
                          'SERTM1', 'PXK', 'PPY2P', 'ETV1', 'ARX', 'CMTM8', 'SCGB2A1',
                          'FXYD2', 'SCGN')

        genes <- Reduce(union, list(alphaMarkers, deltaMarkers,
                                    ductalMarkers, gammaMarkers))

        geneSetExp <- hammers::scExpMat(scObj, genes=genes)
        overlapDF <- CSOA::generateOverlaps(geneSetExp)
        overlapDF <- CSOA::processOverlaps(overlapDF)
        qs2::qs_save(overlapDF, 'inst/extdata/networkPlot.qs2')

        overlapObj <- CSOA:::edgeLists.data.frame(overlapDF)
        degreesDF <- CSOA:::geneDegrees(overlapObj)
        qs2::qs_save(degreesDF, 'inst/extdata/radialPlot.qs2')

        shared <- Reduce(intersect, list(
            rownames(markers[['alpha']]), rownames(markers[['beta']]),
            rownames(markers[['delta']]), rownames(markers[['gamma']])
        ))[seq(12)]

        logsDF <- data.frame(alphaLog = markers[['alpha']][shared, ]$avg_log2FC,
                             betaLog = markers[['beta']][shared, ]$avg_log2FC,
                             deltaLog = markers[['delta']][shared, ]$avg_log2FC,
                             gammaLog = markers[['gamma']][shared, ]$avg_log2FC)
        rownames(logsDF) <- shared
        ranksDF <- apply(logsDF, 2, function(x) rank(-x, ties.method='min'))
        qs2::qs_save(ranksDF, 'inst/extdata/rankPlot.qs2')

        donorMarkers <- LISTO::buildMarkerList(scObj, 'donor')
        markerOverlaps <- LISTO::markerDFListOverlap(markers,
                                                     donorMarkers,
                                                     rownames(scObj))
        riverDF <- hammers::prepAlluvial(markerOverlaps)
        qs2::qs_save(riverDF, 'inst/extdata/riverPlot.qs2')

        celltypes <- names(markers)
        mat <- do.call(rbind, lapply(celltypes, function(i){
            sapply(celltypes, function(j)
                length(intersect(rownames(markers[[i]]),
                                 rownames(markers[[j]]))))
        }))
        rownames(mat) <- celltypes
        colnames(mat) <- celltypes
        qs2::qs_save(mat, 'inst/extdata/tilePlot.qs2')

        df <- Seurat::FindMarkers(scObj,
                                  group.by='label',
                                  ident.1='beta',
                                  logfc.threshold=0,
                                  min.pct=0)
        df <- df[df$p_val_adj < 0.05, ]
        qs2::qs_save(df, 'inst/extdata/volcanoPlot.qs2')

    }
}

createExtData()
