
test_that("new model matrix", {
    dd <- mtcars
    dd$am <- factor(dd$am)

    dat1 <- head(dd, 15)
    mod <- lm(mpg ~ wt + disp + drat:qsec + am, dat1)

    dat2 <- tail(dd)
    X <- new_modelmatrix(mod, dat2)

    expect_equal(rownames(X), tail(rownames(mtcars)))
    expect_equal(colnames(X), c("(Intercept)", "wt", "disp", "am1", "drat:qsec"))

    X2 <- unname(X)
    expect_equal(X2[, 2], tail(mtcars$wt))
    expect_equal(X2[, 3], tail(mtcars$disp))
    expect_equal(X2[, 4], tail(mtcars$am))
    expect_equal(X2[, 5], tail(mtcars$drat * mtcars$qsec))
})