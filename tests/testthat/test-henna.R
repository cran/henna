test_that("classPlot returns a ggplot object", {
    df <- data.frame(Class = sample(paste0('C', seq(13)), 25, replace=TRUE),
                     Item = paste0('I', seq(25)),
                     Value = runif(25, 0.5, 1))
    p <- classPlot(df)
    expect_equal(length(intersect(is(p), c('gg', 'ggplot2::ggplot'))), 1)

    df <- data.frame(Class = sample(paste0('C', seq(13)), 25, replace=TRUE),
                     Item = sample(paste0('I', seq(21)), 25, replace=TRUE),
                     Value = runif(25, 0.5, 1))
    p <- classPlot(df)
    expect_equal(length(intersect(is(p), c('gg', 'ggplot2::ggplot'))), 1)
})

test_that("correlationPlot returns a ggplot object", {
    mat <- matrix(runif(100, -1, 1), nrow=10)
    colnames(mat) <- paste0('I', seq(10))
    mat <- round(cor(mat), 2)
    p <- correlationPlot(mat)
    expect_equal(length(intersect(is(p), c('gg', 'ggplot2::ggplot'))), 1)
})

test_that("densityPlot returns a ggplot object", {
    x <- c(1, 1, 2, 7, 8, 0, 16, 1, 1.2, 32, 7, 5, 1.1, 1.2, 1.2)
    y <- c(1, 1.1, 0.5, 8, 18, 4, 6, 0.9, 1, 6, -24, -28, 1, 0.8, 0.9)
    z <- round(runif(15, 75, 100), 2)
    df <- data.frame(x, y, z)
    rownames(df) <- paste0('p', rownames(df))
    p <- densityPlot(df)
    expect_equal(length(intersect(is(p), c('gg', 'ggplot2::ggplot'))), 1)
})

test_that("hullPlot works", {
    pointsDF <- data.frame(x = c(1, 2, 4, 7, 10, 12, 13, 15, 16),
                           y = c(1, 1, 2, 3, 3, 2,1, 2, 1))

    p <- hullPlot(pointsDF, xInt=9, yInt=2.5, lineColor='maroon', lineWidth=1,
             lineType='solid')
    expect_equal(length(intersect(is(p), c('gg', 'ggplot2::ggplot'))), 1)


    p <- hullPlot(pointsDF, 'Hull plot', 7, 1.5)
    expect_equal(length(intersect(is(p), c('gg', 'ggplot2::ggplot'))), 1)

    expect_error(hullPlot(pointsDF, 'Hull plot', 1, 2))
    expect_error(hullPlot(pointsDF, 'Hull plot', 4, 2))

    p <- hullPlot(pointsDF, 'Hull plot', 4.1, 2)
    expect_equal(length(intersect(is(p), c('gg', 'ggplot2::ggplot'))), 1)

    expect_error(hullPlot(pointsDF, 'Hull plot', 15.5, 1.5))

    p <- hullPlot(pointsDF, 'Hull plot',15.4, 1.5)
    expect_equal(length(intersect(is(p), c('gg', 'ggplot2::ggplot'))), 1)

    rownames(pointsDF) <- paste0('P', seq(nrow(pointsDF)))
    labelDF <- pointsDF[c('P1', 'P4', 'P9'), ]
    p <- hullPlot(pointsDF, 'Hull plot', 7, 1.5,
                  labelDF=labelDF)
    expect_equal(length(intersect(is(p), c('gg', 'ggplot2::ggplot'))), 1)
})

test_that("networkPlot returns a ggraph object", {
    df <- data.frame(gene1 = paste0('G', c(1, 2, 5, 6, 7, 17)),
                     gene2 = paste0('G', c(2, 5, 8, 11, 11, 11)),
                     rank = c(1, 1, 3, 3, 3, 3))
    p <- networkPlot(df)
    expect_equal(is(p), 'ggraph')
    p <- networkPlot(df, nodeColor='orange', edgeColor='green4')
    expect_equal(is(p), 'ggraph')
})

test_that("radialPlot returns a gg object", {
    valuesDF <- data.frame(Protein = paste0('P', seq(20)),
                            Value = sample(10, 20, replace=TRUE),
                            Group = sample(3, 20, replace=TRUE))
    p <- radialPlot(valuesDF, seed=200, breakDensity=8)
    expect_equal(length(intersect(is(p), c('gg', 'ggplot2::ggplot'))), 1)
})

test_that("rankPlot returns a gg object", {
    df <- do.call(cbind, lapply(seq(30), function(i) sample(10, 10)))
    rownames(df) <- paste('M', seq(10))
    colnames(df) <- paste('R', seq(30))
    p <- rankPlot(df)
    expect_equal(length(intersect(is(p), c('gg', 'ggplot2::ggplot'))), 1)
    p <- rankPlot(df, sigDigits=2, labelScalingFactor=0.85, labelOffset=0.07)
    expect_equal(length(intersect(is(p), c('gg', 'ggplot2::ggplot'))), 1)
})

test_that("riverPlot returns a gg object", {
    df <- data.frame(x = sample(c('a','b', 'c', 'd', 'e', 'f'), 20,
                                replace=TRUE),
                     y = sample(c('p','q', 'r', 's', 't', 'u', 'v', 'w'), 20,
                                replace=TRUE),
                     z = runif(20, 1, 3))
    p <- riverPlot(df)
    expect_equal(length(intersect(is(p), c('gg', 'ggplot2::ggplot'))), 1)
})

test_that("tilePlot returns a gg object", {
    mat <- matrix(round(runif(100, 0, 1), 2), nrow=10)
    rownames(mat) <- paste0('R', seq(10))
    colnames(mat) <- paste0('C', seq(10))
    p <- tilePlot(mat)
    expect_equal(length(intersect(is(p), c('gg', 'ggplot2::ggplot'))), 1)
})

test_that("connectedComponents works", {
    df <- data.frame(
        gene1 = paste0('G', c(1, 2, 6, 7, 8, 9,
                              11, 25, 32, 17, 18)),
        gene2 = paste0('G', c(2, 8, 8, 8, 1, 25,
                              32, 24, 24, 26, 26)))

    comps <- as.numeric(connectedComponents(df)$component)
    expect_identical(comps, c(rep(1, 5), rep(2, 4), 3, 3))
})

test_that("vertexComponents works", {
    df <- data.frame(gene1 = c('A', 'B', 'C', 'A'),
                     gene2 = c('B', 'D', 'F', 'G'),
                     component = c(1, 1, 2, 1))
    expect_identical(vertexComponents(df),
                     as.factor(setNames(c(1, 1, 2, 1, 2, 1),
                                        c('A', 'B', 'C', 'D', 'F', 'G'))))
})

test_that("convexHull works", {
    pointsDF <- data.frame(a = c(1, 2, 2, 3, 3, 4, 5, 6, 8, 6, 7, 8, 6, 8, 10, 3, 1),
                           b = c(2, 3, 4, 8, 5, 6, 5, 4, 8, 11, 13, 14, 2, 1, 2, 14, 9))
    hull <- convexHull(pointsDF)
    rownames(hull) <- NULL
    expectedHull <- data.frame(x = c(10, 8, 1, 1, 3, 8),
                               y = c(2, 1, 2, 9, 14, 14))
    expect_identical(hull, expectedHull)
})

test_that("pointsToSegments works", {
    pointsDF <- data.frame(x = c(1, 2, 4, 7, 10,
                                 12, 13, 15, 16),
                           y = c(1, 1, 2, 3, 3, 2, 1, 2, 1))

    hullIndices <- grDevices::chull(pointsDF[, 1], pointsDF[, 2])
    hull <- convexHull(pointsDF, hullIndices)
    hullSegments <- pointsToSegments(hull)
    expectedHullSegments <- data.frame(x = c(16, 1, 7, 10, 15),
                                       y = c(1, 1, 3, 3, 2),
                                       xEnd = c(1, 7, 10, 15, 16),
                                       yEnd = c(1, 3, 3, 2, 1))
    expect_identical(hullSegments, expectedHullSegments)
})

test_that("isPointOnSeg works", {
    expect_true(isPointOnSeg(2, 3, 1, 2, 3, 4))
    expect_false(isPointOnSeg(2, 3, 1, 2, 3, 8))
    expect_false(isPointOnSeg(4, 5, 1, 2, 3, 4))
})

test_that("isPointOnBoundary works", {
    pointsDF <- data.frame(x = c(1, 2, 4, 7, 10, 12, 13, 15, 16),
                           y = c(1, 1, 2, 3, 3, 2, 1, 2, 1))

    hullIndices <- grDevices::chull(pointsDF[, 1], pointsDF[, 2])
    hull <- convexHull(pointsDF, hullIndices)
    hullSegments <- pointsToSegments(hull)
    expect_false(isPointOnBoundary(2, 3, hullSegments))
    expect_true(isPointOnBoundary(12, 2.6, hullSegments))
})
